module Main where

import Parser
import System.Environment
import System.IO
import Evaluator
import Types
import Monad
import Control.Monad
import Control.Monad.Error
import Control.Arrow (second)
import Data.IORef

main :: IO ()
main = do
  x <- getArgs
  funcEnv <- bindPrimitiveFunctions
  varEnv <- initializeEnv
  case length x of
    0 -> runRepl varEnv funcEnv --no args = start up REPL
    1 -> evalAndPrintRawAndPlainExpr varEnv funcEnv (head x) -- single arg = evaluate as an expression
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
        createPrimitiveBindingMap  = map (second PrimitiveFunc) primitives

        createIOPrimitiveBindingMap :: [(String, LispVal)]
        createIOPrimitiveBindingMap  = map (second IOFunc) ioPrimitives

initializeEnv :: IO EnvIORef
initializeEnv = newIORef [] -- newIORef :: a -> IO (IORef a)

runRepl :: EnvIORef -> EnvIORef -> IO ()
runRepl envIORef funcEnvIORef = loop_ (promptAndReadInput "clisp>> ") (== "quit") (evalAndPrintExpr envIORef funcEnvIORef)

loop_ :: Monad m => m a -> (a -> Bool) -> (a -> m()) -> m ()
loop_ promptFunc terminatePredicate action = do
    input <- promptFunc
    unless (terminatePredicate input) $
        do
            action input
            loop_ promptFunc terminatePredicate action

promptAndReadInput :: String -> IO String
promptAndReadInput msg = outputStr msg >> getLine

evalAndPrintRawAndPlainExpr :: EnvIORef -> EnvIORef -> String -> IO ()
evalAndPrintRawAndPlainExpr envIORef funcEnvIORef x = evalAndPrintRawExpr x >> evalAndPrintExpr envIORef funcEnvIORef x

evalAndPrintExpr :: EnvIORef -> EnvIORef -> String -> IO ()
evalAndPrintExpr envIORef funcEnvIORef expr = evalExpr envIORef funcEnvIORef expr >>= outputStrLn

evalExpr :: EnvIORef -> EnvIORef -> String -> IO String
evalExpr envIORef funcEnvIORef str = runThrowsErrorIO $ (liftM show . eval envIORef funcEnvIORef) =<< liftThrowsError (parseSingleExpr str)

-- used for debugging to see parse result
evalAndPrintRawExpr :: String -> IO ()
evalAndPrintRawExpr = outputStrLn . evalRawExpr

evalRawExpr :: String -> String
evalRawExpr = parseExprRaw

outputStr :: String -> IO ()
outputStr msg = putStr msg >> hFlush stdout

outputStrLn :: String -> IO ()
outputStrLn msg = outputStr $ "\n" ++ msg ++ "\n"
