{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where

import Types
import Control.Monad.Error
import Data.Char (toLower)
import Control.Applicative ((<*>))
import Control.Exception
import Monad
import Data.Maybe (fromJust, isNothing)
import System.IO
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError)
import Parser
import Prelude hiding (read)

-- TODO: add eval, with-open-file, funcall, cond, append, backquoted list

eval :: EnvIORef -> EnvIORef -> LispVal -> ThrowsErrorIO LispVal
-- primitives
eval _        _            x@(Keyword _)                                     = return x
eval _        _            x@(String _)                                      = return x
eval _        _            x@(Number _)                                      = return x
eval _        _            x@(Bool _)                                        = return x
eval envIORef _            (Atom x)                                          = getVar envIORef x
eval envIORef funcEnvIORef (DottedList x y)                                  = eval envIORef funcEnvIORef  $ List [Atom "cons", List x, y]
eval _        _            (List [])                                         = return $ Bool False
-- quoted forms
eval _        _            (List [Atom "quoted", val])                       = return val
-- conditionals
eval envIORef funcEnvIORef (List (Atom "case":forms))                        = caseFunc envIORef funcEnvIORef forms
eval envIORef funcEnvIORef (List [Atom "if", predicate, thenForm, elseForm]) = eval envIORef funcEnvIORef predicate >>= \x ->
                                                                                case x of
                                                                                    Bool True -> eval envIORef funcEnvIORef thenForm
                                                                                    Bool False -> eval envIORef funcEnvIORef elseForm
                                                                                    _ -> throwError $ Default "if predicate did not evaluate to a boolean value"
-- functions
eval envIORef funcEnvIORef (List (Atom "setq":newValue))                     = evalSetq envIORef funcEnvIORef newValue
eval envIORef funcEnvIORef (List (Atom "apply":funcParamsAndBody))           = apply envIORef funcEnvIORef funcParamsAndBody
eval envIORef funcEnvIORef (List (Atom "lambda":paramsAndBody))              = makeLambdaFunc paramsAndBody envIORef funcEnvIORef
eval envIORef funcEnvIORef (List (Atom "defun":nameAndParamsAndBody))        = defun nameAndParamsAndBody envIORef funcEnvIORef
eval envIORef funcEnvIORef (List (Atom "load":loadArgs))                     = loadFile envIORef funcEnvIORef loadArgs
eval envIORef funcEnvIORef (List (Atom "prin1":prin1Args))                   = prin1 envIORef funcEnvIORef =<< evalForms envIORef funcEnvIORef prin1Args
eval envIORef funcEnvIORef (List (Atom "print":printArgs))                   = printFunc envIORef funcEnvIORef =<< evalForms envIORef funcEnvIORef printArgs
eval envIORef funcEnvIORef (List (func:args))                                = do
                                                                                evaledFunc <- getFunc func
                                                                                evaledArgs <- evalForms envIORef funcEnvIORef args
                                                                                -- note we don't pass in the environment here because
                                                                                -- a function carries its own lexical environment
                                                                                evalFunc evaledFunc evaledArgs
                                                                                where getFunc :: LispVal -> ThrowsErrorIO LispVal
                                                                                      getFunc (Atom x) = getVar funcEnvIORef x -- lookup symbols in the func namespace
                                                                                      getFunc x = eval envIORef funcEnvIORef x
eval _        _            x                                                 = throwError (Default (show x))


evalFormsAndReturnLast :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
evalFormsAndReturnLast envIORef funcEnvIORef forms = evalForms envIORef funcEnvIORef forms >>= returnLast

evalForms :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO [LispVal]
evalForms envIORef funcEnvIORef = mapM (eval envIORef funcEnvIORef)

returnLast :: [LispVal] -> ThrowsErrorIO LispVal
returnLast x | null x = return (Bool False)
             | otherwise = (return . last) x

--------------------------------------
-- Functions
--------------------------------------
-- evaluate functions
-- Note: args passed in should be already evaluated
evalFunc :: LispVal -> [LispVal] -> ThrowsErrorIO LispVal
evalFunc (PrimitiveFunc func)                                               args = liftThrowsError $ func args
evalFunc (IOFunc ioFunc)                                                    args = ioFunc args
evalFunc (Func reqParams optParams restParam funcBody closure funcEnvIORef) args = bindReqParamsInClosure >>
                                                                                   bindOptParamsInClosure >>
                                                                                   bindRestParamInClosure >>
                                                                                   evalFuncBodyInClosure
    where
        bindReqParamsInClosure :: ThrowsErrorIO EnvIORef
        bindReqParamsInClosure | length reqParams > length args = throwError (NumArgsMismatch (">= " ++ (show . length) reqParams) args)
                               | length args > length reqParams && isNothing optParams = throwError (NumArgsMismatch ((show . length) reqParams) args)
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
                                (List [defaultVal, Atom sVar]) -> return [(sVar, Bool False), (varName, defaultVal)]
                                defaultVal                       -> return [(varName,defaultVal)]

        listAfterReqAndOpt :: LispVal
        listAfterReqAndOpt = List $ drop getLengthoP listAfterReq
            where
                getLengthoP | isNothing optParams = 0
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
                              | otherwise = evalFormsAndReturnLast closure funcEnvIORef funcBody

evalFunc x _ = throwError (TypeMismatch "Function" x)

-- case
caseFunc :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
caseFunc _ _ [] = throwError (NumArgsMismatch "1" [])
caseFunc envIORef funcEnvIORef (keyForm:clauses) = eval envIORef funcEnvIORef keyForm >>=
                                                   chooseCase clauses
                                                    where
                                                        chooseCase :: [LispVal] -> LispVal -> ThrowsErrorIO LispVal
                                                        chooseCase [] _ = return $ Bool False
                                                        chooseCase [List (Atom "otherwise":forms)] _ =
                                                          evalFormsAndReturnLast envIORef funcEnvIORef forms
                                                        chooseCase [List (List keys:forms)] testKey =
                                                          liftThrowsError (checkIfInList testKey keys) >>= \matchFound ->
                                                              if matchFound then evalFormsAndReturnLast envIORef funcEnvIORef forms
                                                              else chooseCase [] testKey
                                                        chooseCase [List (key:forms)] testKey =
                                                          chooseCase [List (List [key]:forms)] testKey
                                                        chooseCase (List (List keys:forms):remainingClauses) testKey =
                                                          liftThrowsError (checkIfInList testKey keys) >>= \matchFound ->
                                                              if matchFound then evalFormsAndReturnLast envIORef funcEnvIORef forms
                                                              else chooseCase remainingClauses testKey
                                                        chooseCase (List (key:forms):remainingClauses) testKey =
                                                          chooseCase (List (List [key]:forms):remainingClauses) testKey
                                                        chooseCase x _ = throwError (TypeMismatch "List" (head x))

                                                        checkIfInList :: LispVal -> [LispVal] -> ThrowsError Bool
                                                        checkIfInList _ [] = return False
                                                        checkIfInList testKey (key:keys) =
                                                          eql [key, testKey] >>= \eq ->
                                                              case eq of
                                                                  Bool False -> checkIfInList testKey keys
                                                                  _ -> return True


-- setq
evalSetq :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
evalSetq envIORef funcEnvIORef l | length l /= 2 = throwError (NumArgsMismatch "2" l)
                                 | otherwise = case l of
                                     [Atom varName, value] -> setOrCreateVar envIORef varName =<< eval envIORef funcEnvIORef value
                                     x -> throwError (TypeMismatch "Atom" (head x))

-- apply
apply :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
apply _ _ [] = throwError (NumArgsMismatch ">= 1" [])
apply envIORef funcEnvIORef (functionDesignator:spreadableListDesignator) = liftM List (liftM (:) processFunctionDesignator <*> processSpreadableListDesignator) >>=
                                                                            eval envIORef funcEnvIORef
    where processFunctionDesignator :: ThrowsErrorIO LispVal
          processFunctionDesignator = eval envIORef funcEnvIORef functionDesignator

          processSpreadableListDesignator :: ThrowsErrorIO [LispVal]
          processSpreadableListDesignator = evalForms envIORef funcEnvIORef spreadableListDesignator >>= expandSpreadableListDesignator >>= mapM makeQuoted

                                                where
                                                    -- if the last element of the spreadable argument list is a list, extract and append those values
                                                    -- if the last element of the spreadable argument list is a dotted list, extract and append its car
                                                    -- if the last element of the spreadable argument list is anything else, ignore it
                                                    expandSpreadableListDesignator :: [LispVal] -> ThrowsErrorIO [LispVal]
                                                    expandSpreadableListDesignator list =
                                                        if null list then return []
                                                        else case last list of
                                                                    (List y) -> return $ init list ++ y
                                                                    (DottedList y _) -> return $ init list ++ y
                                                                    _ -> return (init list)

                                                    -- we've already eval'ed the args, so make each evaled arg quoted so it won't be eval'ed
                                                    -- when we finally evaluate this function in eval
                                                    makeQuoted :: LispVal -> ThrowsErrorIO LispVal
                                                    makeQuoted x = return $ List [Atom "quoted",x]

-- lambda
makeLambdaFunc :: [LispVal] -> EnvIORef -> EnvIORef -> ThrowsErrorIO LispVal
makeLambdaFunc (List funcParams:funcBody) envIORef funcEnvIORef = do
    closure <- copyEnv envIORef -- a lambda carries its own lexical environment. Param mappings are added to the variable closure.
    funcClosure <- copyEnv funcEnvIORef -- The function closure is copied undisturbed. It is copied though, so if you define any new functions
                                        -- in this lambda's body, they won't disturb the global function namespace when they are finally eval'ed
                                        -- in evalFunc
    optParams <- createOptionalParamList closure
    restParam <- createRestParam closure
    return $ Func createReqParamList optParams restParam funcBody closure funcClosure
    where
          notRestAtom :: LispVal -> Bool
          notRestAtom (Atom "&rest") = False
          notRestAtom _ = True

          -- note that the &optional lambda-list parameter is only used when defining a function (as in the lambda here
          -- or in a defun). When you call the function, you do NOT provide an &optional atom but simply provide the
          -- associated value instead.
          notOptionalAtom :: LispVal -> Bool
          notOptionalAtom (Atom "&optional") = False
          notOptionalAtom _ = True

          createReqParamList :: [String]
          createReqParamList = map show $ takeWhile (\x -> (&&) (notOptionalAtom x) (notRestAtom x)) funcParams

          createOptionalParamList :: EnvIORef -> ThrowsErrorIO (Maybe [String])
          createOptionalParamList closure | null optParams = return Nothing
                                          | length optParams == 1 = throwError (NumArgsMismatch ">= 1" optParams)
                                          | otherwise = liftM Just $ mapM bindOptParamDefaultValues $ tail optParams
                                          where
                                              optParams = takeWhile notRestAtom $ dropWhile notOptionalAtom funcParams

                                              bindOptParamDefaultValues :: LispVal -> ThrowsErrorIO String
                                              bindOptParamDefaultValues (Atom x) = bindMultipleVars closure [(x, Bool False)] >> return x -- bind varName to NIL by default
                                              bindOptParamDefaultValues (List [Atom x]) = bindMultipleVars closure [(x, Bool False)] >> return x -- bind varName to NIL by default
                                              bindOptParamDefaultValues (List [Atom x, val]) = bindMultipleVars closure [(x, val)] >> return x -- don't evaluate val, just bind varName to it
                                              bindOptParamDefaultValues (List [Atom x, val, Atom sVar]) = bindMultipleVars closure [(x, List [val, Atom sVar])] >> return x -- supplied p-param bound to NIL by default
                                              bindOptParamDefaultValues x = throwError (NumArgsMismatch "<= 3" [x])

          createRestParam :: EnvIORef -> ThrowsErrorIO (Maybe String)
          createRestParam closure | null restParam = return Nothing
                                  | length restParam /= 2 = throwError (NumArgsMismatch "2" restParam) -- only one specifier allowed for &rest
                                  | otherwise = case restParam !! 1 of -- &rest can only be followed by a symbol (atom)
                                        (Atom x) -> bindMultipleVars closure [(x, Bool False)] >> (return . Just) x -- bind varName to NIL by default
                                        x -> throwError (TypeMismatch "Atom" x)
                                  where
                                      restParam = dropWhile notRestAtom funcParams
makeLambdaFunc (x:_) _ _ = throwError (TypeMismatch "List" x)
makeLambdaFunc x     _ _ = throwError (NumArgsMismatch "2" x)

-- defun
defun :: [LispVal] -> EnvIORef -> EnvIORef -> ThrowsErrorIO LispVal
defun (Atom funcName:paramsAndBody) envIORef funcEnvIORef = makeLambdaFunc paramsAndBody envIORef funcEnvIORef >>= -- the function name is bound in the function namespace and not the variable namespace!
                                                            setOrCreateVar funcEnvIORef funcName
defun x _ _ = throwError (NumArgsMismatch "2" x)

-- load
--   loads file. this has an effect on the environment (unlike read)
--   if file is not found and
--     a. :if-does-not-exist is nil: nil is returned
--     b. else, an error is shown
loadFile :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
loadFile _        _            []                                                       = throwError (NumArgsMismatch "1" [])
loadFile envIORef funcEnvIORef x@[String _]                                             = open x >>= readFileStream >>=
                                                                                          liftThrowsError . parseListOfExpr >>=
                                                                                          evalFormsAndReturnLast envIORef funcEnvIORef
                                                                                          where
                                                                                              readFileStream :: LispVal -> ThrowsErrorIO String
                                                                                              readFileStream (FileStream h) = liftIO $ hGetContents h
                                                                                              readFileStream y = throwError (TypeMismatch "FileStream" y)
loadFile envIORef funcEnvIORef [x@(String _), Keyword ":if-does-not-exist", Bool False] = loadInputFileStream envIORef funcEnvIORef (Bool False) x
loadFile envIORef funcEnvIORef [x@(String _), Keyword ":if-does-not-exist", _]          = loadInputFileStream envIORef funcEnvIORef (Bool True) x
loadFile _        _            x@[String _, Keyword ":if-does-not-exist"]               = throwError (NumArgsMismatch "3" x)
loadFile _        _            x                                                        = throwError (TypeMismatch "String" (head x))

loadInputFileStream :: EnvIORef -> EnvIORef -> LispVal -> LispVal -> ThrowsErrorIO LispVal
loadInputFileStream envIORef funcEnvIORef ifDoesNotExistVal = createFileStreamForLoad envIORef funcEnvIORef ifDoesNotExistVal ReadMode

createFileStreamForLoad :: EnvIORef -> EnvIORef -> LispVal -> IOMode -> LispVal -> ThrowsErrorIO LispVal
createFileStreamForLoad envIORef funcEnvIORef ifDoesNotExistVal = createFileStream (exceptionHandlerForLoad envIORef funcEnvIORef ifDoesNotExistVal)

exceptionHandlerForLoad :: EnvIORef -> EnvIORef -> LispVal -> Either String Handle -> ThrowsErrorIO LispVal
exceptionHandlerForLoad envIORef funcEnvIORef (Bool ifDoesNotExistVal) x = case x of
                                                                            Left err -> if ifDoesNotExistVal then throwError (Default err)
                                                                                        else return (Bool False)
                                                                            Right h -> liftIO (hGetContents h) >>=
                                                                                       liftThrowsError . parseListOfExpr >>=
                                                                                       evalFormsAndReturnLast envIORef funcEnvIORef
exceptionHandlerForLoad _ _ x _ = throwError (TypeMismatch "Bool" x)

-- primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
                ("cons", cons),                                                   -- exactly two args
                ("eql", eql),                                                     -- exactly two args
                ("atom", atom),                                                   -- exactly one arg
                ("weakEqual", weakEqual)                                        -- NOT a common lisp function. equivalence ignoring types. exactly two args
             ]

-- primitives - numeric ops
numericOp0OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp0OrMoreArgs start optParams l = return . Number =<< liftM (foldl optParams start) (mapM extractNumber l)

numericOp1OrMoreArgs :: Integer -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp1OrMoreArgs _ _ [] = throwError (NumArgsMismatch ">=1" [])
numericOp1OrMoreArgs start optParams l = return . Number =<< liftM (foldl optParams start) (mapM extractNumber l)

-- 1+ and 1-
numericOnePlusOneMinusOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOnePlusOneMinusOp optParams l | length l /= 1 =  throwError (NumArgsMismatch "= 1" l)
                                     | otherwise     = return =<< (\x -> return $ Number $ optParams x 1) =<< extractNumber (head l)

-- mod and rem
numericOpNArgs :: Int -> (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOpNArgs n optParams l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
                             | otherwise     = return . Number =<< return . foldl1 optParams =<< mapM extractNumber l

-- primitives - ops that return a boolean result
-- generic boolean function that we use to build string or numeric specific function
genericBoolOpNArgs :: (LispVal -> ThrowsError a) -> Int -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
genericBoolOpNArgs _ n _ l | length l /= n = throwError (NumArgsMismatch ("= " ++ show n) l)
genericBoolOpNArgs extractFunc _ optParams l = return . Bool =<< (\x -> return $ foldl (f (head x)) True (tail x)) =<< mapM extractFunc l
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
ignorecase :: (String -> String -> Bool) -> String -> String -> Bool
ignorecase optParams x y = optParams (map toLower x) (map toLower y)

-- not
-- CLisp 'not' works with generalized booleans. Returns true only if passed nil.
booleanNotOp :: [LispVal] -> ThrowsError LispVal
booleanNotOp l | length l /= 1 = throwError (NumArgsMismatch "= 1" l)
               | otherwise     = case head l of
                                     Bool False -> return $ Bool True
                                     _          -> return $ Bool False

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

-- primitives - list ops
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
cdr [DottedList _ x] = return x
cdr _ = throwError $ Default "cdr needs a list as an argument"

cons :: [LispVal] -> ThrowsError LispVal
cons [x,List y] = return $ List $ x:y                 -- the second one's car becomes the first one's cdr
cons [z,DottedList x y] = return $ DottedList (z:x) y -- the second one's car becomes the first one's cdr
cons [x, y] = return $ DottedList [x] y                 -- non-list cdr's always make dotted lists
cons x = throwError $ NumArgsMismatch "= 2" x

-- primitives - equivalence ops
eql :: [LispVal] -> ThrowsError LispVal
eql [Atom x, Atom y] = return $ Bool ((==) x y)
eql [Number x, Number y] = return $ Bool ((==) x y)
eql [Bool x, Bool y] = return $ Bool ((==) x y)
eql [String x, String y] = return $ Bool ((==) x y)
eql [List x, List y] | length x /= length y = return $ Bool False
                         | otherwise = return . Bool =<< liftM (all toBool) (mapM eql (zipIntoList x y))
eql [DottedList x1 x2, DottedList y1 y2] = eql [List (x1++[x2]), List (y1++[y2])]
eql _ = return $ Bool False

toBool :: LispVal -> Bool
toBool (Bool z) = z
toBool _ = False

zipIntoList :: [LispVal] -> [LispVal] -> [[LispVal]]
zipIntoList = zipWith (\a funcBody -> a:[funcBody])

weakEqual:: [LispVal] -> ThrowsError LispVal
weakEqual [List x, List y] | length x /= length y = return $ Bool False
                           | otherwise = return . Bool =<< liftM (all toBool) (mapM weakEqual (zipIntoList x y))
weakEqual [DottedList x1 x2, DottedList y1 y2] = weakEqual [List (x1++[x2]), List (y1++[y2])]
weakEqual m@[x, y] = return . Bool . or =<< liftM (:) eqlResult <*> mapM (extractAndCheckPrimitiveEquality x y) primitiveEqualityFunctions
                      where
                            eqlResult :: ThrowsError Bool
                            eqlResult = f =<< eql m

                            f :: LispVal -> ThrowsError Bool
                            f (Bool z) = return z
                            f _ = return False -- this will never happen since eql always returns LispVals of type Bool

                            -- this is a heterogenous list!!
                            primitiveEqualityFunctions :: [Extractor]
                            primitiveEqualityFunctions = [Extractor extractBool, Extractor extractString, Extractor extractNumber]
weakEqual _ = return $ Bool False

-- atom
--   atom == not cons
atom :: [LispVal] -> ThrowsError LispVal
atom [List _] = return $ Bool False
atom [DottedList _ _] = return $ Bool False
atom [_] = return $ Bool True
atom x = throwError (NumArgsMismatch "1" x)

-- primitives - io functions
ioPrimitives :: [(String, [LispVal] -> ThrowsErrorIO LispVal)]
ioPrimitives = [
                ("open",            open),
                ("close",           close),
                ("read",            read)
               ]

-- open
--   only supports :direction :input, :direction :output and :direction :io at the moment
open :: [LispVal] -> ThrowsErrorIO LispVal
open [] = throwError (NumArgsMismatch ">= 1" [])
open [x@(String _)] = createInputFileStream x
open [x@(String _),Keyword ":direction",Keyword ":input"] = createInputFileStream x
open [x@(String _),Keyword ":direction",Keyword ":output"] = createOutputFileStream x
open [x@(String _),Keyword ":direction",Keyword ":io"] = createIOFileStream x
open x = throwError (TypeMismatch "String" (head x))

-- takes a String representing a filepath
createInputFileStream :: LispVal -> ThrowsErrorIO LispVal
createInputFileStream = createFileStreamForOpen ReadMode

-- takes a String representing a filepath
createOutputFileStream :: LispVal -> ThrowsErrorIO LispVal
createOutputFileStream = createFileStreamForOpen WriteMode

-- takes a String representing a filepath
createIOFileStream :: LispVal -> ThrowsErrorIO LispVal
createIOFileStream = createFileStreamForOpen ReadWriteMode

createFileStreamForOpen :: IOMode -> LispVal -> ThrowsErrorIO LispVal
createFileStreamForOpen = createFileStream exceptionHandlerForOpen

exceptionHandlerForOpen :: (Either String Handle -> ThrowsErrorIO LispVal)
exceptionHandlerForOpen x = case x of
                                Left err -> throwError (Default err)
                                Right h -> return $ FileStream h

createFileStream :: (Either String Handle -> ThrowsErrorIO LispVal) -> IOMode -> LispVal -> ThrowsErrorIO LispVal
createFileStream handler ioMode (String filePath) =  openfile filePath ioMode >>= handler
createFileStream _ _ x = throwError (TypeMismatch "String" x)

-- catch IOError thrown by System.IO openFile and return informative message for throwError within the ThrowsErrorIO monad
openfile :: String -> IOMode -> ThrowsErrorIO (Either String Handle)
openfile filePath ioMode = liftIO $ catch
                                    (liftM Right $ openFile filePath ioMode)
                                    (\(e :: IOException) -> return . Left $
                                                                if isAlreadyInUseError e then
                                                                    "file " ++ filePath ++ " already in use"
                                                                else if isDoesNotExistError e then
                                                                    "file " ++ filePath ++ " does not exist"
                                                                else -- isPermissionError
                                                                    "no permission to open file " ++ filePath)

-- close
--   in this implementation, closing an already closed stream is not an error
close :: [LispVal] -> ThrowsErrorIO LispVal
close [] = throwError (NumArgsMismatch "1" [])
close [FileStream x] = liftIO $ hClose x >> return (Bool True)
close x = throwError (TypeMismatch "FileStream" (head x))

-- read
--   read is given a stream. it reads from the stream and creates an object that that data represents
--   it does nothing to the environment
read :: [LispVal] -> ThrowsErrorIO LispVal
read [] = read [FileStream stdin]
read [FileStream x] = liftIO (hGetLine x) >>= liftThrowsError . parseSingleExpr
read x = throwError (TypeMismatch "FileStream or []" (head x))

-- prin1
prin1 :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
prin1 _ _ [obj] = liftIO $ putStr (show obj) >> return obj
prin1 envIORef funcEnvIORef [obj, filestream] = eval envIORef funcEnvIORef filestream >>= prin1'
                                        where prin1' :: LispVal -> ThrowsErrorIO LispVal
                                              prin1' (FileStream h) = do
                                                isFileWritable <- liftIO $ hIsWritable h
                                                if isFileWritable then liftIO $ hPutStr h (show obj) >> return obj
                                                else throwError (Default "not an output stream")
                                              prin1' z = throwError (TypeMismatch "FileStream" z)
prin1 _ _ _ = throwError (Default "incorrect parameters for prin1")

-- print
--   same as prin1 except prints a newline at the beginning and a space at the end
printFunc :: EnvIORef -> EnvIORef -> [LispVal] -> ThrowsErrorIO LispVal
printFunc _ _ [obj] = liftIO $ putStr ("\n" ++ show obj ++ " ") >> return obj
printFunc envIORef funcEnvIORef [obj, filestream] = eval envIORef funcEnvIORef filestream >>= printFunc'
                                        where printFunc' :: LispVal -> ThrowsErrorIO LispVal
                                              printFunc' (FileStream h) = do
                                                isFileWritable <- liftIO $ hIsWritable h
                                                if isFileWritable then liftIO $ hPutStr h ("\n" ++ show obj ++ " ") >> return obj
                                                else throwError (Default "not an output stream")
                                              printFunc' z = throwError (TypeMismatch "FileStream" z)
printFunc _ _ _ = throwError (Default "incorrect parameters for print")

--------------------------------------
-- helper functions
--------------------------------------
extractAndCheckPrimitiveEquality :: LispVal -> LispVal -> Extractor -> ThrowsError Bool
extractAndCheckPrimitiveEquality x y (Extractor extractFunc) = catchError checkEquality
                                                                (const (return False))
                                                                where
                                                                    checkEquality :: ThrowsError Bool
                                                                    checkEquality = do
                                                                                        extractedX <- extractFunc x;
                                                                                        extractedY <- extractFunc y;
                                                                                        return $ extractedX == extractedY

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
