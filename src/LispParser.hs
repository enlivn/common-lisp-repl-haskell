module LispParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Types
import Control.Monad (liftM)

readExpr :: String -> String
readExpr inp = case parse parseExpr "lisp" inp of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

-- An Atom is a letter or symbol followed by any number of letters, digits,
-- or symbols
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> specialSymbols
    rest <- many1 (letter <|> specialSymbols <|> digit)
    let atom = first : rest
    case atom of
        "#t" -> return $ Bool True
        "#f" -> return $ Bool False
        _ -> return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    s <- many $ many1 (noneOf "\\\"") <|> escapeSpecial
    _ <- char '"'
    return $ String (concat s)

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseNumber <|> parseAtom

ignoreSpaces :: Parser ()
ignoreSpaces = skipMany1 space
