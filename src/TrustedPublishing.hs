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
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (find)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.URI

example :: LBS.ByteString
example = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTc2ODg3Nzg2MiwiaXNzIjoiaHR0cHM6Ly90b2tlbi5hY3Rpb25zLmdpdGh1YnVzZXJjb250ZW50LmNvbSIsInJlcG9zaXRvcnlfaWQiOiIxMjM0NSIsInJlcG9zaXRvcnlfb3duZXJfaWQiOiIxMzIiLCJlbnZpcm9ubWVudCI6bnVsbH0.D3KBONnXwjXyX_PTLcSVjm6TBWFULFyUsr3aTlgdnvQ"

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
        case findIssuer unverifiedIssuer trustedPublishers of
            Nothing -> throwError JWT.JWTNotInIssuer
            Just p -> pure p
    verified <-
        JWT.verifyJWT
            (JWT.defaultJWTValidationSettings (expectedAudience ==))
            keystore
            decoded
    pure (publisherValidation verified)

findIssuer :: JWT.StringOrURI -> [Publisher result] -> Maybe (Publisher result)
findIssuer iss issuers = find (\(Publisher i _ _) -> i == iss) issuers

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
        JWT.StringOrURI ->
        ks ->
        (parsed -> result) ->
        Publisher result

instance Functor Publisher where
    fmap f (Publisher iss keys validate) = Publisher iss keys (f . validate)

data GithubClaims = GithubClaims
    { repositoryId :: Text
    , repositoryOwnerId :: Text
    , environment :: Maybe Text
    , jwtClaims :: JWT.ClaimsSet
    }
    deriving (Show)

instance JWT.HasClaimsSet GithubClaims where
    claimsSet f s = fmap (\a' -> s{jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON GithubClaims where
    parseJSON = withObject "GithubClaims" $ \o -> do
        repositoryId <- o .: "repository_id"
        repositoryOwnerId <- o .: "repository_owner_id"
        environment <- o .:? "environment"
        jwtClaims <- parseJSON (Object o)
        pure GithubClaims{repositoryId, repositoryOwnerId, environment, jwtClaims}

github :: Publisher GithubClaims
github =
    Publisher
        "https://token.actions.githubusercontent.com"
        (OpenIdDiscoveryUri $ fromJust $ parseURI "https://token.actions.githubusercontent.com/.well-known/openid-configuration")
        id

generic :: Publisher JWT.ClaimsSet
generic =
    Publisher
        "some publisher"
        (undefined :: JWT.JWKSet)
        id

somePublishers :: [Publisher (Either GithubClaims JWT.ClaimsSet)]
somePublishers = [Left <$> github, Right <$> generic]

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
