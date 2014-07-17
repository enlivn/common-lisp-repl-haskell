module Main where

import Parser
import System.Environment
import System.IO
import Types
import Evaluator
import Control.Monad (liftM)

outputStr :: String -> IO ()
outputStr msg = putStr msg >> hFlush stdout

outputStrLn :: String -> IO ()
outputStrLn msg = putStrLn msg >> hFlush stdout

promptAndReadInput :: String -> IO String
promptAndReadInput msg = outputStr msg >> getLine

evalExpr :: [String] -> String
evalExpr str = (extractVal . handleError) $ liftM show $ (readExpr $ str !! 0) >>= eval

evalAndPrintExpr :: [String] -> IO ()
evalAndPrintExpr = (outputStrLn . evalExpr)

-- used for debugging to see parse result
evalRawExpr :: [String] -> String
evalRawExpr = readExprRaw . (!! 0)

evalAndPrintRawExpr :: [String] -> IO ()
evalAndPrintRawExpr = (outputStrLn . evalRawExpr)

evalAndPrintRawAndPlainExpr :: String -> IO ()
evalAndPrintRawAndPlainExpr x = evalAndPrintRawExpr [x] >> evalAndPrintExpr [x]

loop_ :: Monad m => m a -> (a -> Bool) -> (a -> m()) -> m ()
loop_ promptFunc terminatePred action = do
    input <- promptFunc
    if (terminatePred input) then
      return ()
    else do
      action input
      loop_ promptFunc terminatePred action

runRepl :: IO ()
runRepl = loop_ (promptAndReadInput "clisp>> ") (== "quit") (evalAndPrintRawAndPlainExpr)

main :: IO ()
main = do
  x <- getArgs
  case (length x) of
    0 -> runRepl
    1 -> do
            (outputStrLn . evalRawExpr) x
            (outputStrLn . evalExpr) x
    _ -> putStrLn "Only 0 or 1 inputs allowed"
