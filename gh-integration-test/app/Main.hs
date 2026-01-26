{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import System.Exit
import TrustedPublishing

jwtFilePath :: FilePath
jwtFilePath = ".jwt-token"

main :: IO ()
main = do
    jwtbytes <- LBS.fromStrict . BS8.strip <$> BS8.readFile jwtFilePath
    claims <-
        getPublisher
            "https://github.com/spencerjanssen/trusted-publishing"
            [github]
            jwtbytes
            >>= \case
                Left err -> do
                    putStrLn $ "JWT verification failed: " ++ show err
                    exitFailure
                Right claims -> pure claims
    putStrLn $ "JWT verified successfully. Claims: " ++ show claims
