module Evaluator where

import Types
import Control.Monad.Error
import Control.Monad

eval :: LispVal -> ThrowsError LispVal
eval x@(String _) = return x
eval x@(Number _) = return x
eval x@(Bool _) = return x
eval (List [Atom "quoted", val]) = return val
eval (List (Atom func:rest)) = maybe (throwError (NotAFunction "Unrecognized function: " func)) (=<< mapM eval rest) (lookup func primitives)
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybe default_val action maybe_val
-- if maybe_val is Just x, evaluates (action x) and returns the result
-- if maybe_val is Nothing, returns default_val

primitives :: [(String, ([LispVal] -> ThrowsError LispVal))]
primitives = [
                ("+",         numericOp0OrMoreArgs 0 (+)),  -- zero or more args
                ("*",         numericOp0OrMoreArgs 1 (*)),  -- zero or more args
                ("-",         numericOp1OrMoreArgs 0 (-)),           -- one or more args
                ("/",         numericOp1OrMoreArgs 1 div),           -- one or more args
                ("1+",        numericOpNArgs 1 (+)),                 -- exactly one arg
                ("1-",        numericOpNArgs 1 (-)),                 -- exactly one arg
                ("mod",       numericOpNArgs 2 mod),                 -- exactly two args
                ("remainder", numericOpNArgs 2 rem)                  -- exactly two args
             ]

numericOp0OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp0OrMoreArgs start op l = return . Number =<< (liftM (foldl op start) (mapM extractNumber l))

numericOp1OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp1OrMoreArgs _ _ [] = throwError (NumArgsMismatch ">=1" [])
numericOp1OrMoreArgs start op l = return . Number =<< (liftM (foldl op start) (mapM extractNumber l))

numericOpNArgs :: Int -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOpNArgs n op l | length l /= n = throwError (NumArgsMismatch "= 1" l)
                      | otherwise     = return . Number =<< (liftM (foldl op 1) (mapM extractNumber l))

extractNumber :: LispVal -> ThrowsError Integer
extractNumber (Number x) = return x
--extractNumber (String x) = case reads x :: [(Integer, String)] of
--    [] ->  throwError (TypeMismatch "Integer" (String ""))
--    y -> return $ (fst . head) y
--extractNumber (List [x]) = extractNumber x
extractNumber x = throwError (TypeMismatch "Integer" x)
