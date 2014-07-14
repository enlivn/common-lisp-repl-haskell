module Main where

import Parser
import System.Environment

main :: IO ()
main = do
  x <- getArgs
  (putStrLn . readExprRaw . (!! 0)) $ x
  (putStrLn . showVal . readExpr . (!! 0)) $ x
