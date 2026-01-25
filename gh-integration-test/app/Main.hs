module Main where

import Data.ByteString.Lazy qualified as LBS
import System.Exit
import System.IO
import TrustedPublishing qualified

jwtFilePath :: FilePath
jwtFilePath = ".jwt-token"

main :: IO ()
main = do
    rawjwt <- LBS.readFile jwtFilePath
    result <-
        TrustedPublishing.getPublisher
            [TrustedPublishing.github]
            rawjwt
    case result of
        Left err -> do
            hPutStrLn stderr ("JWT verification failed: " ++ show err)
            exitFailure
        Right claims -> do
            putStrLn ("JWT verified successfully. Claims: " ++ show claims)
