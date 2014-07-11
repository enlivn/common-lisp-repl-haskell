module Types where

import Text.ParserCombinators.Parsec hiding (spaces)

escapeSpecial :: Parser String
escapeSpecial = do
    _ <- char '\\' --left-factored this out of escapeQuote and escapeOthers
    escapeQuote <|> escapeOthers

-- escape quotes
-- e.g. '"i say \"this is a valid string.\""'
escapeQuote :: Parser String
escapeQuote = do
    s <- char '"'
    return [s]

-- escape \n, \r, \t, \\
escapeOthers :: Parser String
escapeOthers = do
    s <- oneOf "nrt\\"
    return $ '\\':[s]

specialSymbols :: Parser Char
specialSymbols = oneOf "!$%&|*+-/:<=?>@^_~#"

-- An Atom is a letter or symbol followed by any number of letters, digits,
-- or symbols
-- A DottedList is the CAR-CADR type Lisp list
data LispVal =  Atom String |
                List [LispVal] |
                DottedList [LispVal] LispVal |
                Number Integer |
                Bool Bool |
                String String
                deriving Show
