{-# LANGUAGE ExistentialQuantification #-}
module Types where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Data.IORef

type Env = [(String, IORef LispVal)]

type EnvIORef = IORef Env

type ThrowsError = Either LispError

-- An Atom is a letter or symbol followed by any number of letters, digits,
-- or symbols
-- A DottedList is the CAR-CADR type Lisp list
data LispVal =  Atom String |
                Number Integer |
                Bool Bool |
                String String |
                List [LispVal] |
                DottedList [LispVal] LispVal |
                PrimitiveFunc ([LispVal] -> ThrowsError LispVal) |
                Func {requiredParams :: [String],
                      optionalParams :: Maybe [String],
                      restParams :: Maybe String,
                      body :: [LispVal],
                      closureEnv :: EnvIORef} -- the function carries around its own environment with bound params. this gives us lexical scoping

data LispError = NumArgsMismatch String [LispVal] |
                 TypeMismatch String LispVal |
                 ParseError ParseError |
                 NotAFunction String String |
                 UnboundVar String String |
                 Default String

data Extractor = forall a . Eq a => Extractor (LispVal -> ThrowsError a)

-- Used to print values
instance Show LispVal where
    show (Atom a) = a
    show (Number n) = show n
    show (Bool True) = "t"
    show (Bool False) = "NIL"
    show (String s) = '"' : s ++ "\""
    show (List []) = "NIL"
    show (List l) = "(" ++ showLispValList l ++ ")"
    show (DottedList h t) = "(" ++ showLispValList h ++ " . " ++ show t ++ ")"
    show (PrimitiveFunc _) = "<primitive function>"
    show (Func reqParams optParams restParam _ _) = "(lambda required("
                                ++ unwords reqParams ++ ")"
                                ++ (case optParams of
                                        Nothing -> ""
                                        Just x -> " &optional(" ++ unwords x ++ ") ")
                                ++ (case restParam of
                                        Nothing -> ""
                                        Just x -> " &rest(" ++ x ++ ") ")
                                ++ " ...)"

showLispValList :: [LispVal] -> String
showLispValList = unwords . map show

instance Show LispError where
    show (NumArgsMismatch expectedNumArgs foundArgs) = "Incorrect Number of Args - expected: " ++ expectedNumArgs ++ ", found: " ++ show foundArgs
    show (TypeMismatch expectedType actualLispVal)   = "Invalid type - expected: " ++ expectedType ++ ", found: " ++ show actualLispVal
    show (ParseError err)                            = "Parse error - " ++ show err
    show (NotAFunction msg func)                     = "Invalid function - message: " ++ msg ++ ", function name: " ++ func
    show (UnboundVar msg varName)                    = "Unbound variable - message: " ++ msg ++ ", variable name: " ++ varName
    show (Default msg)                               = "Error - message: " ++ msg

instance Error LispError where
    strMsg = Default
    noMsg = Default "An error has occurred."

-- Make all wrongs Right
handleError :: ThrowsError String -> ThrowsError String
handleError monadAction = catchError monadAction (return . show)

-- Only handles Right!!!
-- Left is a programmer error
-- Make sure we never give it a Left by using handleError for possible Lefts
extractVal :: ThrowsError a -> a
extractVal (Right a) = a
