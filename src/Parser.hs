module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Types
import Control.Monad.Error

parseSingleExpr :: String -> ThrowsError LispVal
parseSingleExpr = parseExpression lispParser

parseListOfExpr :: String -> ThrowsError [LispVal]
parseListOfExpr = parseExpression (sepEndBy (skipMany space >> lispParser) (skipMany space))

parseExpression :: Parser a -> String -> ThrowsError a
parseExpression parser inp = case parse parser "lisp" inp of
    Left err -> throwError (ParseError err)
    Right val -> return val

lispParser :: Parser LispVal
lispParser = try parseSpecialAtom <|>
             try parseNumber <|>
             parseAtom <|>
             parseString <|>
             parseSingleQuoted <|>
             parseListCommon

-- functions "1+" and "1-"
parseSpecialAtom :: Parser LispVal
parseSpecialAtom = do
    x <- char '1'
    y <- oneOf "+-"
    return $ Atom $ x:[y]

-- "+1" and "-1" are valid numbers. They are not atoms.
parseNumber :: Parser LispVal
parseNumber = do
    y <- option '+' (oneOf "+-")
    x <- many1 digit
    case y of
        '+' -> return $ (Number . read) x
        _   -> return $ (Number . read) $ y:x

-- An Atom is a letter or symbol followed by any number of letters, digits,
-- or symbols
-- However, "+1" and "-1" are valid numbers. They are not atoms.
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> specialSymbols
    rest <- many (letter <|> specialSymbols <|> digit)
    let atom = first : rest
    case atom of
        "t" -> return $ Bool True
        "nil" -> return $ Bool False
        (':':_) -> return $ Keyword atom
        _ -> return $ Atom atom

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    s <- many $ many1 (noneOf "\\\"") <|> escapeSpecial
    _ <- char '"'
    return $ String (concat s)

    where
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

parseSingleQuoted :: Parser LispVal
parseSingleQuoted = do
    _ <- char '\''
    x <- lispParser
    return $ List $ Atom "quoted" : [x]

specialSymbols :: Parser Char
specialSymbols = oneOf "!$%&|*+-/:<=?>@^_~#"

ignoreSpaces :: Parser ()
ignoreSpaces = skipMany1 space

-- list parsing with factored out left grammar. No need for backtracking
-- in lispParser
parseListCommon :: Parser LispVal
parseListCommon = do
    _ <- char '('
    h <- sepEndBy (skipMany space >> lispParser) (skipMany space)
    x <- parseDotted h <|> parseNormal h
    _ <- char ')'
    return x

    where
        parseNormal :: [LispVal] -> Parser LispVal
        parseNormal h = return $ List h

        parseDotted :: [LispVal] -> Parser LispVal
        parseDotted h = do
            t <- char '.' >> ignoreSpaces >> lispParser
            return $ DottedList h t

-- show the raw form of the resulting LispVal
-- for debugging use only
parseExprRaw :: String -> String
parseExprRaw inp = case parse lispParser "lisp" inp of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

-- parseExpr with unfactored left grammar.
-- only for comparison with parseExpr
parseExprUnfactored :: String -> String
parseExprUnfactored inp = case parse lispParserUnfactored "lisp" inp of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

-- lispParser with unfactored left grammar. Needs backtracking with the
-- try block as shown below
lispParserUnfactored :: Parser LispVal
lispParserUnfactored = parseAtom <|>
            parseNumber <|>
            parseString <|>
            -- parseSingleQuoted, parseListUnfactored and parseDottedListUnfactored allow recursion
            -- becase they call lispParser
            parseSingleQuoted <|>
            do
                _ <- char '('
                -- the try allows backtracking in case the parseListUnfactored fails
                x <- try parseListUnfactored <|> parseDottedListUnfactored
                _ <- char ')'
                return x

parseListUnfactored :: Parser LispVal
parseListUnfactored = liftM List $ sepBy lispParser (skipMany space)

-- A DottedList is the CAR-CADR type Lisp list
parseDottedListUnfactored :: Parser LispVal
parseDottedListUnfactored = do
    h <- endBy lispParser space
    t <- char '.' >> ignoreSpaces >> lispParser
    return $ DottedList h t
