module Main where

import Parser
import System.Environment
import System.IO
import Evaluator
import Monad
import Control.Monad.Error

outputStr :: String -> IO ()
outputStr msg = putStr msg >> hFlush stdout

outputStrLn :: String -> IO ()
outputStrLn msg = outputStr $ msg ++ "\n"

promptAndReadInput :: String -> IO String
promptAndReadInput msg = outputStr msg >> getLine

evalExpr :: EnvIORef -> [String] -> IO String
evalExpr envIORef str = runThrowsErrorIO $ (liftM show . eval envIORef) =<< (liftThrowsError $ parseExpr $ str !! 0)

evalAndPrintExpr :: EnvIORef -> [String] -> IO ()
evalAndPrintExpr envIORef expr = evalExpr envIORef expr >>= outputStrLn

-- used for debugging to see parse result
evalRawExpr :: [String] -> String
evalRawExpr = parseExprRaw . (!! 0)

evalAndPrintRawExpr :: [String] -> IO ()
evalAndPrintRawExpr = (outputStrLn . evalRawExpr)

evalAndPrintRawAndPlainExpr :: EnvIORef -> String -> IO ()
evalAndPrintRawAndPlainExpr envIORef x = evalAndPrintRawExpr [x] >> evalAndPrintExpr envIORef [x]

loop_ :: Monad m => m a -> (a -> Bool) -> (a -> m()) -> m ()
loop_ promptFunc terminatePred action = do
    input <- promptFunc
    if (terminatePred input) then
      return ()
    else do
      action input
      loop_ promptFunc terminatePred action

runRepl :: EnvIORef -> IO ()
runRepl envIORef = loop_ (promptAndReadInput "clisp>> ") (== "quit") (evalAndPrintRawAndPlainExpr envIORef)

main :: IO ()
main = do
  x <- getArgs
  case (length x) of
    0 -> runRepl =<< initEnv --no args = start up REPL
    1 -> (flip evalAndPrintRawAndPlainExpr) (x !! 0) =<< initEnv -- single arg = evaluate as an expression
    _ -> putStrLn "Only 0 or 1 arguments allowed"
