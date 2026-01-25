module Main where

import Control.Exception
import System.Exit
import System.IO

jwtFilePath :: FilePath
jwtFilePath = ".jwt-token"

main :: IO ()
main = do
    result <- try readJWTToken
    case result of
        Left e -> do
            putStrLn $ "Error reading JWT token: " ++ show (e :: IOError)
            exitFailure
        Right token -> do
            putStrLn "JWT token successfully read"
            putStrLn $ "Token length: " ++ show (length token)
            putStrLn "Token starts with: ey..." -- JWT tokens always start with "ey"
            -- Here you would use the token for your trusted publishing operations
            putStrLn "Ready to perform trusted publishing operations"

readJWTToken :: IO String
readJWTToken = do
    exists <- doesFileExist jwtFilePath
    if not exists
        then ioError $ userError $ "JWT token file not found: " ++ jwtFilePath
        else do
            content <- readFile jwtFilePath
            let token = strip content
            if null token
                then ioError $ userError "JWT token file is empty"
                else return token
  where
    strip = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')

doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    result <- try $ openFile path ReadMode
    case result of
        Left (_ :: IOError) -> return False
        Right h -> do
            hClose h
            return True
