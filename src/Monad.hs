module Monad where

import Data.IORef
import Types
import Control.Monad.Error

type ThrowsErrorIO = ErrorT LispError IO

debug :: String -> ThrowsErrorIO ()
debug = liftIO . putStrLn

-- this is a one-way street!! you CANNOT escape back to pure code (except by unsafe IO)
liftThrowsError :: ThrowsError a -> ThrowsErrorIO a
liftThrowsError (Left a) = throwError a
liftThrowsError (Right a) = return a

runThrowsErrorIO :: ThrowsErrorIO String -> IO String
runThrowsErrorIO x = runErrorT x >>= (return . extractVal . handleError) -- runErrorT :: ErrorT e m a -> m (Either e a)

isVarDefined :: EnvIORef -> String -> IO Bool
isVarDefined envIORef varName = readIORef envIORef >>= (return . (lookup varName)) >>= return . (maybe (False) (const True)) -- readIORef :: IORef a -> IO a

-- get value of an existing var
-- throws UnboundVar error if var does not exist
getVar :: EnvIORef -> String -> ThrowsErrorIO LispVal
getVar envIORef varName = do
        env <- liftIO $ readIORef envIORef
        maybe (throwError $ UnboundVar "cannot get unbound variable" varName) (liftIO . readIORef) (lookup varName env)

-- set an existing var to a new value
-- throws UnboundVar error if var does not exist
setVar :: EnvIORef -> String -> LispVal -> ThrowsErrorIO LispVal
setVar envIORef varName newValue = do
        -- debug $ " setting " ++ varName ++ " to " ++ show newValue
        env <- liftIO $ readIORef envIORef
        case lookup varName env of
            Nothing -> throwError $ UnboundVar "cannot set unbound variable" varName
            Just _ -> liftIO $ setExistingVar envIORef varName newValue

bindMultipleVars :: EnvIORef -> [(String, LispVal)] -> ThrowsErrorIO EnvIORef
bindMultipleVars envIORef bindings = mapM (uncurry $ setOrCreateVar envIORef) bindings >> return envIORef

-- same as setVar except if var doesn't exist, creates it
setOrCreateVar :: EnvIORef -> String -> LispVal -> ThrowsErrorIO LispVal
setOrCreateVar envIORef varName newValue = do
        -- debug $ " setting " ++ varName ++ " to " ++ show newValue
        env <- liftIO $ readIORef envIORef
        case lookup varName env of
            Nothing -> liftIO $ createAndSetNewVar envIORef varName newValue
            Just _ -> liftIO $ setExistingVar envIORef varName newValue

copyEnv ::EnvIORef -> ThrowsErrorIO EnvIORef
copyEnv e = liftIO $ readIORef e >>= newIORef

setExistingVar :: EnvIORef -> String -> LispVal -> IO LispVal
setExistingVar envIORef varName newValue = do
    removeExistingBinding >>= \x -> createAndSetNewVar x varName newValue
    where
        removeExistingBinding :: IO EnvIORef
        removeExistingBinding = do
            env <- readIORef envIORef
            writeIORef envIORef (filter (\(x,_) -> x /= varName) env) >> return envIORef

createAndSetNewVar :: EnvIORef -> String -> LispVal -> IO LispVal
createAndSetNewVar envIORef varName newValue = do
        env <- liftIO $ readIORef envIORef
        newCreatedValue <- newIORef newValue
        writeIORef envIORef ([(varName, newCreatedValue)] ++ env) >> readIORef newCreatedValue -- writeIORef :: IORef a -> a -> IO ()
