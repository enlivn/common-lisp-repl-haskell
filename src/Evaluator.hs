module Evaluator where

import Types
import Control.Monad.Error
import Data.Char (toLower)
import Control.Applicative ((<*>))
import Monad

eval :: EnvIORef -> LispVal -> ThrowsErrorIO LispVal
eval _ x@(String _) = return x
eval _ x@(Number _) = return x
eval _ x@(Bool _) = return x
-- quoted forms. Note that we don't evaluate the symbol
eval _ (List [Atom "quoted", val]) = return val
-- if clause
eval envIORef (List [Atom "if", predicate, thenForm, elseForm]) = (eval envIORef predicate) >>= \x ->
    case x of
        Bool True -> eval envIORef thenForm
        Bool False -> eval envIORef elseForm
        _ -> throwError $ Default "if predicate did not evaluate to a boolean value"
-- functions
eval envIORef (List (Atom func:rest)) = (maybe (throwError (NotAFunction "Unrecognized function" func))
                                               (\primitiveFunc -> (liftThrowsError . primitiveFunc) =<< (mapM (eval envIORef) rest)))
                                               =<< return (lookup func primitives)
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybe default_val action maybe_val
-- if maybe_val is Just x, evaluates (action x) and returns the result
-- if maybe_val is Nothing, returns default_val

primitives :: [(String, ([LispVal] -> ThrowsError LispVal))]
primitives = [
                -- numeric operations
                ("+",            numericOp0OrMoreArgs 0 (+)),                     -- zero or more args
                ("*",            numericOp0OrMoreArgs 1 (*)),                     -- zero or more args
                ("-",            numericOp1OrMoreArgs 0 (-)),                     -- one or more args
                ("/",            numericOp1OrMoreArgs 1 div),                     -- one or more args
                                                                                  -- TODO: / is NOT in line with CLisp specs,
                                                                                  --       which also provide for a Ratio being returned
                ("/=",           boolNumericOpOneOrMoreArgs (/=)),                -- one or more args
                ("=",            boolNumericOpOneOrMoreArgs (==)),                -- one or more args
                ("<",            boolNumericOpOneOrMoreArgs (<)),                 -- one or more args
                (">",            boolNumericOpOneOrMoreArgs (>)),                 -- one or more args
                ("<=",           boolNumericOpOneOrMoreArgs (<=)),                -- one or more args
                (">=",           boolNumericOpOneOrMoreArgs (>=)),                -- one or more args
                ("1+",           numericOnePlusOneMinusOp (+)),                   -- exactly one arg
                ("1-",           numericOnePlusOneMinusOp (-)),                   -- exactly one arg
                ("mod",          numericOpNArgs 2 mod),                           -- exactly two args
                ("rem",          numericOpNArgs 2 rem),                           -- exactly two args

                -- boolean operations
                ("not",          booleanNotOp),                                   -- exactly one arg
                ("and",          booleanAndOp),                                   -- zero or more args
                ("or",           booleanOrOp),                                    -- zero or more args

                -- string operations
                ("string=",      boolStringOpTwoArgs (==)),                       -- exactly two args
                ("string-equal", boolStringOpTwoArgs (ignorecase (==))),          -- exactly two args
                ("string/=",     boolStringOpTwoArgs (/=)),                       -- exactly two args
                ("string<",      boolStringOpTwoArgs (<)),                        -- exactly two args
                ("string-lessp", boolStringOpTwoArgs (ignorecase (<))),           -- exactly two args
                ("string>",      boolStringOpTwoArgs (>)),                        -- exactly two args
                ("string-greaterp", boolStringOpTwoArgs (ignorecase (>))),        -- exactly two args
                ("string<=",     boolStringOpTwoArgs (<=)),                       -- exactly two args
                ("string-not-greaterp", boolStringOpTwoArgs (ignorecase (<=))),   -- exactly two args
                ("string>=",     boolStringOpTwoArgs (>=)),                       -- exactly two args
                ("string-not-lesserp", boolStringOpTwoArgs (ignorecase (>=))),    -- exactly two args

                -- list operations
                ("car", car),                                                     -- exactly one arg
                ("cdr", cdr),                                                     -- exactly one arg
                ("cons", cons),                                                   -- exactly one arg
                ("eql", eql),                                                     -- exactly two args
                ("weak_equal", weak_equal)                                        -- NOT a common lisp function. equivalence ignoring types. exactly two args
             ]

--------------------------------------
-- ops that return a numeric result.
--------------------------------------
numericOp0OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp0OrMoreArgs start op l = return . Number =<< (liftM (foldl op start) (mapM extractNumber l))

numericOp1OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp1OrMoreArgs _ _ [] = throwError (NumArgsMismatch ">=1" [])
numericOp1OrMoreArgs start op l = return . Number =<< (liftM (foldl op start) (mapM extractNumber l))

-- 1+ and 1-
numericOnePlusOneMinusOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOnePlusOneMinusOp op l | length l /= 1 =  throwError (NumArgsMismatch "= 1" l)
                              | otherwise     = return =<< (\x -> return $ Number $ op x 1) =<< extractNumber (head l)

-- mod and rem
numericOpNArgs :: Int -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOpNArgs n op l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
                      | otherwise     = return . Number =<< (\x -> return $ foldl op (head x) (tail x)) =<< (mapM extractNumber l)

--------------------------------------
-- ops that return a boolean result.
--------------------------------------
-- generic boolean function that we use to build string or numeric specific function
genericBoolOpNArgs :: (LispVal -> ThrowsError a) -> Int -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
genericBoolOpNArgs _ n _ l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
genericBoolOpNArgs extractFunc _ op l = return . Bool =<< (\x -> return $ foldl (f (head x)) True (tail x)) =<< (mapM extractFunc l)
    where f _ False _ = False
          f x _ y | x `op` y = True
                  | otherwise = False

-- >, <, >=, <=, =, /=
boolNumericOpOneOrMoreArgs :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
boolNumericOpOneOrMoreArgs _ [] = throwError (NumArgsMismatch ">=1" [])
boolNumericOpOneOrMoreArgs op l  = genericBoolOpNArgs extractNumber (length l) op l -- (length l) effectively bypasses length check in genericBoolOpNArgs

-- string=, string/=, string<, string>, string<=, string>=
boolStringOpTwoArgs :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
boolStringOpTwoArgs = genericBoolOpNArgs extractString 2

-- for string-equal, string-lessp, string-greaterp, string-not-lesserp, string-not-greaterp
ignorecase :: (String -> String -> Bool) -> (String -> String -> Bool)
ignorecase op x y = (op (map toLower x) (map toLower y))

-- not
-- CLisp 'not' works with generalized booleans. Returns true only if passed nil.
booleanNotOp :: [LispVal] -> ThrowsError LispVal
booleanNotOp l | length l /= 1 = throwError (NumArgsMismatch "= 1" l)
              | otherwise     = case (head l) of
                (Bool False) -> return $ Bool True
                _            -> return $ Bool False

-- or
booleanOrOp :: [LispVal] -> ThrowsError LispVal
booleanOrOp l = return $ foldl f (Bool False) l
    where f :: LispVal -> LispVal -> LispVal
          f (Bool False) x = x
          f y _ = y

-- and
booleanAndOp :: [LispVal] -> ThrowsError LispVal
booleanAndOp l = return $ foldl f (Bool True) l
    where f :: LispVal -> LispVal -> LispVal
          f (Bool False) _ = Bool False
          f _ y = y

--------------------------------------
-- list ops
--------------------------------------
car :: [LispVal] -> ThrowsError LispVal
car [List []] = return $ Bool False
car [List [x]] = return x
car [List (x:_)] = return x
car [DottedList [] _] = return $ Bool False
car [DottedList [x] _] = return x
car [DottedList (x:_) _] = return x
car _ = throwError $ Default "car needs a list as an argument"

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List []] = return $ List []
cdr [List [_]] = return $ List []
cdr [List (_:x)] = return $ List x
cdr [DottedList _ x] = return $ x
cdr _ = throwError $ Default "cdr needs a list as an argument"

cons :: [LispVal] -> ThrowsError LispVal
cons [x,(List y)] = return $ List $ x:y                 -- the second one's car becomes the first one's cdr
cons [z,(DottedList x y)] = return $ DottedList (z:x) y -- the second one's car becomes the first one's cdr
cons [x, y] = return $ DottedList [x] y                 -- non-list cdr's always make dotted lists
cons x = throwError $ NumArgsMismatch "= 2" x

--------------------------------------
-- Equivalence ops
--------------------------------------
eql :: [LispVal] -> ThrowsError LispVal
eql [(Atom x), (Atom y)] = return $ Bool ((==) x y)
eql [(Number x), (Number y)] = return $ Bool ((==) x y)
eql [(Bool x), (Bool y)] = return $ Bool ((==) x y)
eql [(String x), (String y)] = return $ Bool ((==) x y)
eql [(List x), (List y)] | length x /= length y = return $ Bool False
                         | otherwise = return . Bool =<< liftM (all toBool) (mapM eql (zipIntoList x y))

eql [(DottedList x1 x2), (DottedList y1 y2)] = eql [List (x1++[x2]), List (y1++[y2])]
eql _ = return $ Bool $ False

toBool :: LispVal -> Bool
toBool (Bool z) = z
toBool _ = False

zipIntoList :: [LispVal] -> [LispVal] -> [[LispVal]]
zipIntoList = zipWith (\a b -> a:[b])

-- use applicative <*>
weak_equal:: [LispVal] -> ThrowsError LispVal
weak_equal [(List x), (List y)] | length x /= length y = return $ Bool False
                                | otherwise = return . Bool =<< liftM (all toBool) (mapM weak_equal (zipIntoList x y))
weak_equal [(DottedList x1 x2), (DottedList y1 y2)] = weak_equal [List (x1++[x2]), List (y1++[y2])]
weak_equal m@[x, y] = return . Bool . any id  =<< (liftM (:) eqlResult) <*> mapM (extractAndCheckPrimitiveEquality x y) primitiveEqualityFunctions
                      where
                            eqlResult :: ThrowsError Bool
                            eqlResult = f =<< eql m

                            f :: LispVal -> ThrowsError Bool
                            f (Bool z) = return z
                            f _ = return False -- this will never happen since eql always returns LispVals of type Bool

                            -- this is a heterogenous list!!
                            primitiveEqualityFunctions :: [Extractor]
                            primitiveEqualityFunctions = [Extractor extractBool, Extractor extractString, Extractor extractNumber]
weak_equal _ = return $ Bool False

--------------------------------------
-- helper functions
--------------------------------------
extractAndCheckPrimitiveEquality :: LispVal -> LispVal -> Extractor -> ThrowsError Bool
extractAndCheckPrimitiveEquality x y (Extractor extractFunc) = do
    catchError (do{
        extractedX <- extractFunc x;
        extractedY <- extractFunc y;
        return $ extractedX == extractedY})
        (const (return False))

extractBool :: LispVal -> ThrowsError Bool
extractBool (Bool x) = return x
extractBool x = throwError (TypeMismatch "Bool" x)

extractString :: LispVal -> ThrowsError String
extractString (String x) = return x
extractString (Number x) = return $ show x
extractString (Bool x) | x = return "t"
                       | otherwise = return "NIL"
extractString x = throwError (TypeMismatch "String" x)

extractNumber :: LispVal -> ThrowsError Integer
extractNumber (Number x) = return x
-- strictly speaking, Common Lisp does not allow strings to be interpreted as numbers without using parse-integer
extractNumber (String x) = case reads x :: [(Integer, String)] of
    [] ->  throwError (TypeMismatch "Integer" (String ""))
    y -> return $ (fst . head) y
extractNumber (List [x]) = extractNumber x
extractNumber x = throwError (TypeMismatch "Integer" x)
