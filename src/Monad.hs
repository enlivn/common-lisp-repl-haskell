module Monad where

import Data.IORef
import Types
import Control.Monad.Error

type Env = [(String, IORef LispVal)]

type EnvIORef = IORef Env

initEnv :: IO EnvIORef
initEnv = newIORef [] -- newIORef :: a -> IO (IORef a)

type ThrowsErrorIO = ErrorT LispError IO

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
        maybe (throwError $ UnboundVar " cannot get unbound variable: " varName) (liftIO . readIORef) (lookup varName env)

-- set an existing var to a new value
-- throws UnboundVar error if var does not exist
setVar :: EnvIORef -> String -> LispVal -> ThrowsErrorIO LispVal
setVar envIORef varName newValue = do
        env <- liftIO $ readIORef envIORef
        maybe (throwError $ UnboundVar " cannot set unbound variable: " varName) (liftIO . (setExistingVar newValue)) (lookup varName env)

-- same as setVar except if var doesn't exist, creates it
defineVar :: EnvIORef -> String -> LispVal -> ThrowsErrorIO LispVal
defineVar envIORef varName newValue = do
        env <- liftIO $ readIORef envIORef
        maybe (liftIO $ createAndSetNewVar env) (liftIO . (setExistingVar newValue)) (lookup varName env)
        where
            createAndSetNewVar :: Env -> IO LispVal
            createAndSetNewVar env = do
                newCreatedValue <- newIORef newValue
                writeIORef envIORef (env ++ [(varName, newCreatedValue)]) >> readIORef newCreatedValue -- writeIORef :: IORef a -> a -> IO ()

bindMultipleVars :: EnvIORef -> [(String, LispVal)] -> ThrowsErrorIO EnvIORef
bindMultipleVars envIORef bindings = mapM (uncurry $ defineVar envIORef) bindings >> return envIORef

setExistingVar :: LispVal -> IORef LispVal -> IO LispVal
setExistingVar newValue x = liftIO (writeIORef x newValue) >> readIORef x
