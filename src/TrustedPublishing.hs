{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TrustedPublishing where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Crypto.JOSE qualified as JOSE
import Crypto.JOSE.Types
import Crypto.JWT qualified as JWT
import Data.Aeson
import Data.Attoparsec.Text qualified as Attoparsec
import Data.ByteString.Lazy qualified as LBS
import Data.Char
import Data.Foldable (find)
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.URI

getPublisher ::
    JWT.StringOrURI ->
    [Publisher result] ->
    LBS.ByteString ->
    IO (Either JWT.JWTError result)
getPublisher expectedAudience trustedPublishers jwtbytes = JOSE.runJOSE do
    decoded <- JOSE.decodeCompact jwtbytes
    munverifiedIssuer <- (^. JWT.claimIss) <$> JWT.unsafeGetJWTClaimsSet decoded
    unverifiedIssuer <- case munverifiedIssuer of
        Nothing -> throwError JWT.JWTNotInIssuer
        Just i -> pure i
    (Publisher publisherIssuer keystore publisherValidation) <-
        case find (\(Publisher i _ _) -> i == unverifiedIssuer) trustedPublishers of
            Nothing -> throwError JWT.JWTNotInIssuer
            Just p -> pure p
    let validationSettings =
            JWT.defaultJWTValidationSettings (expectedAudience ==)
                & JWT.issuerPredicate .~ (publisherIssuer ==)
                & JWT.allowedSkew .~ 30
    verified <- JWT.verifyJWT validationSettings keystore decoded
    pure (publisherValidation verified)

data Publisher result where
    Publisher ::
        ( JWT.HasClaimsSet parsed
        , FromJSON parsed
        , JWT.VerificationKeyStore
            (JWT.JOSE JWT.JWTError IO)
            (JWT.JWSHeader JOSE.RequiredProtection)
            parsed
            ks
        ) =>
        -- | Issuer
        JWT.StringOrURI ->
        -- | Key store
        ks ->
        -- | Adapt the parsed claims to some result type
        (parsed -> result) ->
        Publisher result

instance Functor Publisher where
    fmap f (Publisher iss keys validate) = Publisher iss keys (f . validate)

data GithubWorkflowRef = GithubWorkflowRef
    { owner :: Text
    , repo :: Text
    , workflowFileName :: Text
    , ref :: Text
    }
    deriving (Show, Eq)

instance FromJSON GithubWorkflowRef where
    parseJSON = withText "GithubWorkflowRef" $ \t ->
        case Attoparsec.parseOnly wflowRefParser t of
            Left err -> fail $ "Failed to parse GithubWorkflowRef: " ++ err
            Right ref' -> pure ref'

-- pypi and crates use a regular expression here, unclear what is best
wflowRefParser :: Attoparsec.Parser GithubWorkflowRef
wflowRefParser = do
    owner <- Attoparsec.takeWhile1 ghIdOk
    void $ Attoparsec.string "/"
    repo <- Attoparsec.takeWhile1 ghIdOk
    void $ Attoparsec.string "@"
    void $ Attoparsec.string ".github/workflows/"
    workflowFileName <- Attoparsec.takeWhile1 (\c -> c /= '/' && c /= '@')
    void $ Attoparsec.string "@"
    ref <- Attoparsec.takeWhile1 (/= '@')
    Attoparsec.endOfInput
    pure GithubWorkflowRef{owner, repo, workflowFileName, ref}
  where
    ghIdOk c =
        isAsciiLower c
            || isAsciiUpper c
            || (c >= '0' && c < '9')
            || c == '-'
            || c == '_'
            || c == '.'

data GithubClaims = GithubClaims
    { repositoryId :: Text
    , repository :: Text
    , repositoryOwnerId :: Text
    , repositoryOwner :: Text
    , environment :: Maybe Text
    , workflowRef :: GithubWorkflowRef
    , jobWorkflowRef :: GithubWorkflowRef
    , jwtClaims :: JWT.ClaimsSet
    }
    deriving (Show)

data GithubTrustRelationship = GithubTrustRelationship
    { trustedRepositoryId :: Text
    , trustedRepository :: Text
    , trustedRepositoryOwnerId :: Text
    , trustedRepositoryOwner :: Text
    , trustedEnvironment :: Maybe Text
    }
    deriving (Show)

githubClaimsAreTrusted ::
    GithubClaims ->
    GithubTrustRelationship ->
    Bool
githubClaimsAreTrusted claims trust =
    repositoryId claims == trustedRepositoryId trust
        && repositoryOwnerId claims == trustedRepositoryOwnerId trust
        && case trustedEnvironment trust of
            Nothing -> True
            Just env -> environment claims == Just env

instance JWT.HasClaimsSet GithubClaims where
    claimsSet f s = fmap (\a' -> s{jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON GithubClaims where
    parseJSON = withObject "GithubClaims" $ \o -> do
        repositoryId <- o .: "repository_id"
        repository <- o .: "repository"
        repositoryOwnerId <- o .: "repository_owner_id"
        repositoryOwner <- o .: "repository_owner"
        environment <- o .:? "environment"
        workflowRef <- o .: "workflow_ref"
        jobWorkflowRef <- o .: "job_workflow_ref"
        jwtClaims <- parseJSON (Object o)
        pure
            GithubClaims
                { repositoryId
                , repository
                , repositoryOwnerId
                , repositoryOwner
                , environment
                , workflowRef
                , jobWorkflowRef
                , jwtClaims
                }

github :: Publisher GithubClaims
github =
    Publisher
        "https://token.actions.githubusercontent.com"
        (OpenIdDiscoveryUri $ fromJust $ parseURI "https://token.actions.githubusercontent.com/.well-known/openid-configuration")
        id

newtype OpenIdDiscoveryUri = OpenIdDiscoveryUri URI

instance
    (MonadIO m, JWT.HasJWSHeader h, JWT.HasClaimsSet cs) =>
    JWT.VerificationKeyStore m (h p) cs OpenIdDiscoveryUri
    where
    getVerificationKeys h claims (OpenIdDiscoveryUri discoveryUri) = do
        case preview (JWT.kid . _Just . JWT.param) h of
            Nothing -> pure []
            Just kid -> liftIO (findKey kid) <&> maybeToList
      where
        findKey :: Text -> IO (Maybe JWT.JWK)
        findKey kid' =
            handle (\(_ :: SomeException) -> pure Nothing) $ do
                discoveryRequest <- setRequestCheckStatus <$> requestFromURI discoveryUri
                OidcDiscovery{jwksUri} <- getResponseBody <$> httpJSON @_ @OidcDiscovery discoveryRequest
                keyRequest <- setRequestCheckStatus <$> requestFromURI jwksUri
                response <- getResponseBody <$> httpJSON @_ @JWT.JWKSet keyRequest
                keys <- JWT.getVerificationKeys h claims response
                pure $ find (\j -> view JWT.jwkKid j == Just kid') keys

data OidcDiscovery = OidcDiscovery
    { issuer :: Text
    , jwksUri :: URI
    }

instance FromJSON OidcDiscovery where
    parseJSON = withObject "OidcDiscovery" $ \o -> do
        issuer <- o .: "issuer"
        jwksUri <- o .: "jwks_uri"
        pure OidcDiscovery{issuer, jwksUri}
