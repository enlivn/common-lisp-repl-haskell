{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where

import Types
import Control.Monad.Error
import Data.Char (toLower)
import Control.Applicative ((<*>))
import Control.Exception
import Monad
import Data.Maybe (fromJust)
import System.IO
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError)
import Parser
import Prelude hiding (read)

-- TODO: add print, apply, eval, with-open-file, atom, funcall, cons, cond, case, append, backquoted list

eval :: EnvIORef -> LispVal -> ThrowsErrorIO LispVal
-- primitives
eval _ x@(String _) = return x
eval _ x@(Number _) = return x
eval _ x@(Bool _) = return x
eval envIORef (Atom x) = getVar envIORef x
-- quoted forms. Note that we don't evaluate the symbol
eval _ (List []) = return $ Bool False
eval _ (List [Atom "quoted", val]) = return val
-- if clause
eval envIORef (List [Atom "if", predicate, thenForm, elseForm]) = (eval envIORef predicate) >>= \x ->
    case x of
        Bool True -> eval envIORef thenForm
        Bool False -> eval envIORef elseForm
        _ -> throwError $ Default "if predicate did not evaluate to a boolean value"
-- functions
eval envIORef (List (Atom "setq":newValue)) = evalSetq envIORef newValue
eval envIORef (List (Atom "lambda":paramsAndBody)) = makeLambdaFunc paramsAndBody envIORef
eval envIORef (List (Atom "defun":nameAndParamsAndBody)) = defun nameAndParamsAndBody envIORef
eval envIORef (List (Atom "load":loadArgs)) = loadFunc envIORef loadArgs
eval envIORef (List (func:args)) = do
    evaledFunc <- eval envIORef func
    evaledArgs <- mapM (eval envIORef) args
    evalFunc evaledFunc evaledArgs -- note we don't pass in the environment here because a function carries its own lexical environment

--------------------------------------
-- Setq evaluation
--------------------------------------
evalSetq :: EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
evalSetq envIORef l | length l /= 2 = throwError (NumArgsMismatch "2" l)
                    | otherwise = case l of
                        [Atom varName, value] -> setOrCreateVar envIORef varName value
                        x -> throwError (TypeMismatch "Atom" (head x))

--------------------------------------
-- Function evaluation
--------------------------------------
makeLambdaFunc :: [LispVal] -> EnvIORef -> ThrowsErrorIO LispVal
makeLambdaFunc (List funcParams:funcBody) envIORef = do
    closure <- copyEnv envIORef -- a lambda carries its own lexical environment
    optParams <- createOptionalParamList closure
    restParam <- createRestParam closure
    return $ Func createReqParamList optParams restParam funcBody closure
    where
          notRestAtom :: LispVal -> Bool
          notRestAtom (Atom "&rest") = False
          notRestAtom _ = True

          notOptionalAtom :: LispVal -> Bool
          notOptionalAtom (Atom "&optional") = False
          notOptionalAtom _ = True

          createReqParamList :: [String]
          createReqParamList = map show $ takeWhile (\x -> ((&&) (notOptionalAtom x) (notRestAtom x))) funcParams

          createOptionalParamList :: EnvIORef -> ThrowsErrorIO (Maybe [String])
          createOptionalParamList closure = if null optParams then return Nothing
                                            else if length optParams == 1 then throwError (NumArgsMismatch ">= 1" optParams)
                                            else liftM Just $ mapM bindOptParamDefaultValues $ tail optParams
                                            where
                                                optParams = takeWhile (notRestAtom) $ dropWhile (notOptionalAtom) funcParams

                                                bindOptParamDefaultValues :: LispVal -> ThrowsErrorIO String
                                                bindOptParamDefaultValues (Atom x) = bindMultipleVars closure [(x, Bool False)] >> return x -- bind varName to NIL by default
                                                bindOptParamDefaultValues (List [Atom x]) = bindMultipleVars closure [(x, Bool False)] >> return x -- bind varName to NIL by default
                                                bindOptParamDefaultValues (List [Atom x, val]) = bindMultipleVars closure [(x, val)] >> return x -- don't evaluate val, just bind varName to it
                                                bindOptParamDefaultValues (List [Atom x, val, Atom sVar]) = bindMultipleVars closure [(x, List [val, Atom sVar])] >> return x -- supplied p-param bound to NIL by default
                                                bindOptParamDefaultValues x = throwError (NumArgsMismatch "<= 3" [x])

          createRestParam :: EnvIORef -> ThrowsErrorIO (Maybe String)
          createRestParam closure = if null restParam then return Nothing
                                    else if length restParam /= 2 then throwError (NumArgsMismatch "2" restParam) -- only one specifier allowed for &rest
                                    else case restParam !! 1 of -- &rest can only be followed by a symbol (atom)
                                        (Atom x) -> bindMultipleVars closure [(x, Bool False)] >> (return . Just) x -- bind varName to NIL by default
                                        x -> throwError (TypeMismatch "Atom" x)
                                    where
                                        restParam = dropWhile notRestAtom funcParams
makeLambdaFunc (x:_) _  = throwError (TypeMismatch "List" x)
makeLambdaFunc x     _  = throwError (NumArgsMismatch "2" x)

-- Note: args passed in should be already evaluated
evalFunc :: LispVal -> [LispVal] -> ThrowsErrorIO LispVal
evalFunc (PrimitiveFunc func) args = liftThrowsError $ func args
evalFunc (IOFunc ioFunc) args = ioFunc args
evalFunc (Func reqParams optParams restParam funcBody closure) args = bindReqParamsInClosure >>
                                                                      bindOptParamsInClosure >>
                                                                      bindRestParamInClosure >>
                                                                      evalFuncBodyInClosure
    where
        bindReqParamsInClosure :: ThrowsErrorIO EnvIORef
        bindReqParamsInClosure | length reqParams > length args = throwError (NumArgsMismatch (">= " ++ (show . length) reqParams) args)
                               | otherwise = bindMultipleVars closure (zip reqParams args)

        listAfterReq :: [LispVal]
        listAfterReq = drop (length reqParams) args

        bindOptParamsInClosure :: ThrowsErrorIO EnvIORef
        bindOptParamsInClosure = case optParams of
            Nothing -> return closure
            Just x -> liftM concat (zipWithM f x listAfterReq) >>= bindMultipleVars closure >> bindOptVarsWithDefaultAndPParamsButNoArgs x
            where f :: String -> LispVal -> ThrowsErrorIO [(String, LispVal)]
                  f varName newValue = getVar closure varName >>= \currentVal ->
                    case currentVal of
                        (List [_, Atom sVar]) -> return [(sVar, Bool True), (varName, newValue)]
                        _                     -> return [(varName, newValue)]

                  -- for vars that had default values AND supplied p-params but for which no arguments were provided,
                  -- set the vars to the default values and the p-params to NIL
                  -- for vars that had ONLY default values, we've already bound those in makeLambdaFunc. They may have been overriden
                  -- with user-supplied values in f above, but we don't need to do anything more.
                  bindOptVarsWithDefaultAndPParamsButNoArgs :: [String] -> ThrowsErrorIO EnvIORef
                  bindOptVarsWithDefaultAndPParamsButNoArgs vars =  liftM concat (mapM g vars) >>= bindMultipleVars closure
                    where g :: String -> ThrowsErrorIO [(String, LispVal)]
                          g varName = getVar closure varName >>= \currentVal ->
                            case currentVal of
                                (List [defaultVal, (Atom sVar)]) -> return [(sVar, Bool False), (varName, defaultVal)]
                                defaultVal                       -> return [(varName,defaultVal)]

        listAfterReqAndOpt :: LispVal
        listAfterReqAndOpt = List $ drop (getLengthoP) listAfterReq
            where
                getLengthoP | optParams == Nothing = 0
                            | otherwise = length (fromJust optParams)

        bindRestParamInClosure :: ThrowsErrorIO EnvIORef
        bindRestParamInClosure = case restParam of
            Nothing -> return closure
            Just x -> case listAfterReqAndOpt of
                List [] -> return closure
                y -> bindMultipleVars closure [(x, y)] -- override the default bound values if arguments provided

        -- return NIL if the body is empty, else return the result of the last form
        evalFuncBodyInClosure :: ThrowsErrorIO LispVal
        evalFuncBodyInClosure | null funcBody = return $ Bool False
                              | otherwise = mapM (eval closure) funcBody >>= return . last

evalFunc x _ = throwError (TypeMismatch "Function" x)

defun :: [LispVal] -> EnvIORef -> ThrowsErrorIO LispVal
defun (Atom funcName:paramsAndBody) envIORef = setOrCreateVar envIORef funcName =<< makeLambdaFunc paramsAndBody envIORef
defun x _ = throwError (NumArgsMismatch "2" x)

loadFunc :: EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
loadFunc _ [] = throwError (NumArgsMismatch "1" [])
loadFunc envIORef x@[String _] = (open x) >>= readFileStream >>= liftThrowsError . parseListOfExpr >>= mapM (eval envIORef) >>= return . last -- return last form
    where
        readFileStream :: LispVal -> ThrowsErrorIO String
        readFileStream (FileStream h) = liftIO $ hGetContents h
        readFileStream y = throwError (TypeMismatch "FileStream" y)
loadFunc _ x = throwError (TypeMismatch "String" (head x))

-- primitive functions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

-- Primitive Numeric operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
numericOp0OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp0OrMoreArgs start optParams l = return . Number =<< (liftM (foldl optParams start) (mapM extractNumber l))

numericOp1OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp1OrMoreArgs _ _ [] = throwError (NumArgsMismatch ">=1" [])
numericOp1OrMoreArgs start optParams l = return . Number =<< (liftM (foldl optParams start) (mapM extractNumber l))

-- 1+ and 1-
numericOnePlusOneMinusOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOnePlusOneMinusOp optParams l | length l /= 1 =  throwError (NumArgsMismatch "= 1" l)
                              | otherwise     = return =<< (\x -> return $ Number $ optParams x 1) =<< extractNumber (head l)

-- mod and rem
numericOpNArgs :: Int -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOpNArgs n optParams l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
                      | otherwise     = return . Number =<< (\x -> return $ foldl optParams (head x) (tail x)) =<< (mapM extractNumber l)

-- Primitive ops that return a boolean result
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- generic boolean function that we use to build string or numeric specific function
genericBoolOpNArgs :: (LispVal -> ThrowsError a) -> Int -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
genericBoolOpNArgs _ n _ l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
genericBoolOpNArgs extractFunc _ optParams l = return . Bool =<< (\x -> return $ foldl (f (head x)) True (tail x)) =<< (mapM extractFunc l)
    where f _ False _ = False
          f x _ y | x `optParams` y = True
                  | otherwise = False

-- >, <, >=, <=, =, /=
boolNumericOpOneOrMoreArgs :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
boolNumericOpOneOrMoreArgs _ [] = throwError (NumArgsMismatch ">=1" [])
boolNumericOpOneOrMoreArgs optParams l  = genericBoolOpNArgs extractNumber (length l) optParams l -- (length l) effectively bypasses length check in genericBoolOpNArgs

-- string=, string/=, string<, string>, string<=, string>=
boolStringOpTwoArgs :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
boolStringOpTwoArgs = genericBoolOpNArgs extractString 2

-- for string-equal, string-lessp, string-greaterp, string-not-lesserp, string-not-greaterp
ignorecase :: (String -> String -> Bool) -> (String -> String -> Bool)
ignorecase optParams x y = (optParams (map toLower x) (map toLower y))

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

-- Primitive list ops
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

-- Primitive Equivalence ops
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
zipIntoList = zipWith (\a funcBody -> a:[funcBody])

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

ioPrimitives :: [(String, ([LispVal] -> ThrowsErrorIO LispVal))]
ioPrimitives = [
                ("open",            open),
                ("close",           close),
                ("read",            read)
               ]

-- only supports :direction :input, :direction :output and :direction :io at the moment
open :: [LispVal] -> ThrowsErrorIO LispVal
open [] = throwError (NumArgsMismatch ">= 1" [])
open [x@(String _)] = createInputFileStream x
open (x@(String _):Atom ":direction":Atom ":input":_) = createInputFileStream x
open (x@(String _):Atom ":direction":Atom ":output":_) = createOutputFileStream x
open (x@(String _):Atom ":direction":Atom ":io":_) = createIOFileStream x
open x = throwError (TypeMismatch "String" (head x))

-- takes a String representing a filepath
createInputFileStream :: LispVal -> ThrowsErrorIO LispVal
createInputFileStream = createFileStream ReadMode

-- takes a String representing a filepath
createOutputFileStream :: LispVal -> ThrowsErrorIO LispVal
createOutputFileStream = createFileStream WriteMode

-- takes a String representing a filepath
createIOFileStream :: LispVal -> ThrowsErrorIO LispVal
createIOFileStream = createFileStream ReadWriteMode

createFileStream :: IOMode -> LispVal -> ThrowsErrorIO LispVal
createFileStream ioMode (String filePath) =  openfile filePath ioMode >>= \x ->
        case x of
            Left err -> throwError (Default err)
            Right h -> return $ FileStream h
createFileStream _ x = throwError (TypeMismatch "String" x)

-- catch IOError thrown by System.IO openFile and return informative message for throwError
openfile :: String -> IOMode -> ThrowsErrorIO (Either String Handle)
openfile filePath ioMode = liftIO $ catch
                                    (liftM Right $ openFile filePath ioMode)
                                    (\(e :: IOException) -> if (isAlreadyInUseError e) then
                                                                return (Left $ "file " ++ filePath ++ " already in use")
                                                            else if (isDoesNotExistError e) then
                                                                return (Left $ "file " ++ filePath ++ " does not exist")
                                                            else -- isPermissionError
                                                                return (Left $ "no permission to open file " ++ filePath))

-- in this implementation, closing an already closed stream is not an error
close :: [LispVal] -> ThrowsErrorIO LispVal
close [] = throwError (NumArgsMismatch "1" [])
close [FileStream x] = liftIO $ hClose x >> return (Bool True)
close x = throwError (TypeMismatch "FileStream" (head x))

-- read is given a stream. it reads from the stream and creates an object that that data represents
-- it does nothing to the environment
read :: [LispVal] -> ThrowsErrorIO LispVal
read [] = read [FileStream stdin]
read [FileStream x] = (liftIO $ hGetLine x) >>= liftThrowsError . parseSingleExpr
read x = throwError (TypeMismatch "FileStream or []" (head x))

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
