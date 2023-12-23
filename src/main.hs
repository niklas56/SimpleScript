module Main where

import Logic
import Parser
import Types
import Data.Char
import Control.Applicative (Alternative, empty, (<|>), many)
import System.Environment

runFile :: String -> [String] -> IO ()
runFile fileName args = do
  input <- readFile fileName
  case runParser (indentedLinesP 0) (input ++ "\n") of
    Just ("", lines) -> executeC [] [("args", ListVal (StrVal <$> args))] lines >> return ()
    Just (rest, _) -> putStrLn $ "Unparsed: \n" ++ rest
    Nothing -> putStrLn "Invalid input"

debugFile :: String -> IO ()
debugFile fileName = do
  input <- readFile fileName
  print $ runParser (indentedLinesP 0) (input ++ "\n")

main:: IO()
main = do
  (name:restArgs) <- getArgs
  runFile name restArgs