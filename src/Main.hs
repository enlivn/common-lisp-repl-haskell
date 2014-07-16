module Main where

import Parser
import System.Environment
import Types
import Evaluator
import Control.Monad (liftM)

main :: IO ()
main = do
  x <- getArgs
  (putStrLn . readExprRaw . (!! 0)) $ x
  (putStrLn . extractVal) $ handleError $ liftM show $ (readExpr $ x !! 0) >>= eval
