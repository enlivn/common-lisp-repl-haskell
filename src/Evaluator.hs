module Evaluator where

import Types

eval :: LispVal -> LispVal
eval x@(String _) = x
eval x@(Number _) = x
eval x@(Bool _) = x
eval (List [Atom "quoted", val]) = val
eval (List (Atom func:rest)) = maybe (Bool False) ($ map eval rest) (lookup func primitives)

primitives :: [(String, ([LispVal] -> LispVal))]
primitives = [
                ("+", numericNaryOp (+)),
                ("1+", numericNaryOpWith1 (+)),
                ("-", numericNaryOp (-)),
                ("1-", numericNaryOpWith1 (-)),
                ("*", numericNaryOp (*)),
                ("/", numericNaryOp div),
                ("mod", numericNaryOp mod),
                ("quotient", numericNaryOp quot),
                ("remainder", numericNaryOp rem)
             ]

numericNaryOpWith1 :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericNaryOpWith1 op l = Number $ foldl op 1 (map extractNumber l)

numericNaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericNaryOp op l = Number $ foldl1 op (map extractNumber l)

extractNumber :: LispVal -> Integer
extractNumber (Number x) = x
--extractNumber (String x) = case reads x :: [(Integer, String)] of
--    [] -> 0
--    y -> (fst . head) y
--extractNumber (List [x]) = extractNumber x
extractNumber _ = 0
