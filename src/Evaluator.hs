module Evaluator where

import Types
import Control.Monad.Error

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
                -- numeric operations
                ("+",         numericOp0OrMoreArgs 0 (+)),  -- zero or more args
                ("*",         numericOp0OrMoreArgs 1 (*)),  -- zero or more args
                ("-",         numericOp1OrMoreArgs 0 (-)),  -- one or more args
                ("/",         numericOp1OrMoreArgs 1 div),  -- one or more args -- TODO: / is NOT in line with CLisp specs,
                                                                                --       which also provide for a Ratio being returned
                ("/=",        numericOp1OrMoreArgs' (/=)),  -- one or more args
                ("=",         numericOp1OrMoreArgs' (==)),  -- one or more args
                ("<",         numericOp1OrMoreArgs' (<)),   -- one or more args
                (">",         numericOp1OrMoreArgs' (>)),   -- one or more args
                ("<=",        numericOp1OrMoreArgs' (<=)),  -- one or more args
                (">=",        numericOp1OrMoreArgs' (>=)),  -- one or more args
                ("1+",        numericOpNArgs1 (+)),        -- exactly one arg
                ("1-",        numericOpNArgs1 (-)),        -- exactly one arg
                ("mod",       numericOpNArgs 2 mod),        -- exactly two args
                ("rem",       numericOpNArgs 2 rem),        -- exactly two args

                -- boolean operations
                ("not",       booleanNotOp),                -- exactly one arg
                ("and",       booleanAndOp),                -- zero or more args
                ("or",        booleanOrOp)                 -- zero or more args
             ]

numericOp0OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp0OrMoreArgs start op l = return . Number =<< (liftM (foldl op start) (mapM extractNumber l))

numericOp1OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp1OrMoreArgs _ _ [] = throwError (NumArgsMismatch ">=1" [])
numericOp1OrMoreArgs start op l = return . Number =<< (liftM (foldl op start) (mapM extractNumber l))

-- no start value provided. the start value is the first element of the [LispVal] sent in (only if it's a number, otherwise
-- extractNumber protects us by throwing a type mismatch error)
numericOp1OrMoreArgs' :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numericOp1OrMoreArgs'  _ [] = throwError (NumArgsMismatch ">=1" [])
numericOp1OrMoreArgs' op l = return . Bool =<< (\x -> return $ foldl (f (head x)) True (tail x)) =<< (mapM extractNumber l)
    where f :: Integer -> Bool -> Integer -> Bool
          f _ False _ = False
          f x _ y | x `op` y = True
                  | otherwise = False

numericOpNArgs1 :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOpNArgs1 op l | length l /= 1 =  throwError (NumArgsMismatch "= 1" [])
                     | otherwise     = return =<< (\x -> return $ Number $ op x 1) =<< extractNumber (head l)

numericOpNArgs :: Int -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOpNArgs n op l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
                      | otherwise     = return . Number =<< (\x -> return $ foldl op (head x) (tail x)) =<< (mapM extractNumber l)

-- CLisp 'not' works with generalized booleans. Returns true only if passed nil.
booleanNotOp :: [LispVal] -> ThrowsError LispVal
booleanNotOp l | length l /= 1 = throwError (NumArgsMismatch "= 1" l)
              | otherwise     = case (head l) of
                (Bool False) -> return $ Bool True
                _            -> return $ Bool False

booleanOrOp :: [LispVal] -> ThrowsError LispVal
booleanOrOp l = return $ foldl f (Bool False) l
    where f :: LispVal -> LispVal -> LispVal
          f (Bool False) x = x
          f y _ = y

booleanAndOp :: [LispVal] -> ThrowsError LispVal
booleanAndOp l = return $ foldl f (Bool True) l
    where f :: LispVal -> LispVal -> LispVal
          f (Bool False) _ = Bool False
          f _ y = y

extractString :: LispVal -> ThrowsError String
extractString (String x) = return x
extractString x = throwError (TypeMismatch "String" x)

extractNumber :: LispVal -> ThrowsError Integer
extractNumber (Number x) = return x
--extractNumber (String x) = case reads x :: [(Integer, String)] of
--    [] ->  throwError (TypeMismatch "Integer" (String ""))
--    y -> return $ (fst . head) y
--extractNumber (List [x]) = extractNumber x
extractNumber x = throwError (TypeMismatch "Integer" x)
