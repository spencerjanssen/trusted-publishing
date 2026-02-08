{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.String
import Data.Text.IO qualified as T
import System.Environment
import System.Exit
import TrustedPublishing

jwtFilePath :: FilePath
jwtFilePath = ".jwt-token"

getEnvT :: (IsString a) => String -> IO a
getEnvT = fmap fromString . getEnv

main :: IO ()
main = do
    audience <- getEnvT "AUDIENCE"
    trustedRepository <- getEnvT "REPO"
    trustedRepositoryId <- getEnvT "REPO_ID"
    trustedRepositoryOwner <- getEnvT "REPO_OWNER"
    trustedRepositoryOwnerId <- getEnvT "REPO_OWNER_ID"
    trustedWorkflowFilename <- getEnvT "WORKFLOW_FILENAME"
    print ("Audience: ", audience)

    jwtbytes <- LBS.fromStrict . BS8.strip <$> BS8.readFile jwtFilePath
    claims <-
        getPublisher
            audience
            [github]
            jwtbytes
            >>= \case
                Left err -> do
                    putStrLn $ "JWT verification failed: " ++ show err
                    exitFailure
                Right claims -> pure claims
    putStrLn $ "JWT verified successfully. Claims: " ++ show claims
    let trustVerified =
            githubClaimsAreTrusted
                claims
                GithubTrustRelationship
                    { trustedRepository
                    , trustedWorkflowFilename
                    , trustedEnvironment = Nothing
                    , trustedRepositoryId
                    , trustedRepositoryOwnerId
                    , trustedRepositoryOwner
                    }
    case trustVerified of
        Trusted -> putStrLn "Claims are trusted for this repository and workflow."
        Untrusted reasons -> do
            putStrLn "Claims are NOT trusted for this repository and workflow. Reasons:"
            mapM_ T.putStrLn reasons
            exitFailure
