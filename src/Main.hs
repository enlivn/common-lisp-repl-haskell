module Main where

import Parser
import System.Environment
import System.IO
import Evaluator
import Types
import Monad
import Control.Monad.Error
import Data.IORef

main :: IO ()
main = do
  x <- getArgs
  case (length x) of
    0 -> runRepl =<< bindPrimitiveFunctions --no args = start up REPL
    1 -> (flip evalAndPrintRawAndPlainExpr) (x !! 0) =<< bindPrimitiveFunctions -- single arg = evaluate as an expression
    _ -> putStrLn "Error: Only 0 or 1 arguments allowed."

--------------------------------------
-- helper functions
--------------------------------------
bindPrimitiveFunctions :: IO EnvIORef
bindPrimitiveFunctions =  bindMultipleVars' createIOPrimitiveBindingMap =<< bindMultipleVars' createPrimitiveBindingMap  =<< initializeEnv
    where
        bindMultipleVars' :: [(String, LispVal)] -> EnvIORef -> IO EnvIORef
        bindMultipleVars' bindings envIORef = runErrorT (bindMultipleVars envIORef bindings) >>= \x -> case x of
            Right env -> return env -- leaving out Left because that signals programmer error

        createPrimitiveBindingMap :: [(String, LispVal)]
        createPrimitiveBindingMap  = map (\(x,y) -> (x, PrimitiveFunc y)) primitives

        createIOPrimitiveBindingMap :: [(String, LispVal)]
        createIOPrimitiveBindingMap  = map (\(x,y) -> (x, IOFunc y)) ioPrimitives

initializeEnv :: IO EnvIORef
initializeEnv = newIORef [] -- newIORef :: a -> IO (IORef a)

runRepl :: EnvIORef -> IO ()
runRepl envIORef = loop_ (promptAndReadInput "clisp>> ") (== "quit") (evalAndPrintExpr envIORef)

loop_ :: Monad m => m a -> (a -> Bool) -> (a -> m()) -> m ()
loop_ promptFunc terminatePredicate action = do
    input <- promptFunc
    if (terminatePredicate input) then
      return ()
    else do
      action input
      loop_ promptFunc terminatePredicate action

promptAndReadInput :: String -> IO String
promptAndReadInput msg = outputStr msg >> getLine

evalAndPrintRawAndPlainExpr :: EnvIORef -> String -> IO ()
evalAndPrintRawAndPlainExpr envIORef x = evalAndPrintRawExpr x >> evalAndPrintExpr envIORef x

evalAndPrintExpr :: EnvIORef -> String -> IO ()
evalAndPrintExpr envIORef expr = evalExpr envIORef expr >>= outputStrLn

evalExpr :: EnvIORef -> String -> IO String
evalExpr envIORef str = runThrowsErrorIO $ (liftM show . eval envIORef) =<< (liftThrowsError $ parseSingleExpr $ str)

-- used for debugging to see parse result
evalAndPrintRawExpr :: String -> IO ()
evalAndPrintRawExpr = (outputStrLn . evalRawExpr)

evalRawExpr :: String -> String
evalRawExpr = parseExprRaw

outputStr :: String -> IO ()
outputStr msg = putStr msg >> hFlush stdout

outputStrLn :: String -> IO ()
outputStrLn msg = outputStr $ msg ++ "\n"
