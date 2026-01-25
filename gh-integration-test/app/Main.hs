{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import System.Exit
import System.IO
import TrustedPublishing qualified

jwtFilePath :: FilePath
jwtFilePath = ".jwt-token"

main :: IO ()
main = do
    rawjwt <- BS8.strip <$> BS8.readFile jwtFilePath
    result <-
        TrustedPublishing.getPublisher
            "https://github.com/spencerjanssen/trusted-publishing"
            [TrustedPublishing.github]
            (LBS.fromStrict rawjwt)
    case result of
        Left err -> do
            hPutStrLn stderr ("JWT verification failed: " ++ show err)
            exitFailure
        Right claims -> do
            putStrLn ("JWT verified successfully. Claims: " ++ show claims)
