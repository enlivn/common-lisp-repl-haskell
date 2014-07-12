module Main where

import LispParser
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr $ head args)
    --putStrLn (readExprUnfactored $ head args)
