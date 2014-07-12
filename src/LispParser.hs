module LispParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Types
import Control.Monad (liftM)

readExpr :: String -> String
readExpr inp = case parse parseExpr "lisp" inp of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseAtom <|>
            parseNumber <|>
            parseString <|>
            parseSingleQuoted <|>
            parseListCommon

-- An Atom is a letter or symbol followed by any number of letters, digits,
-- or symbols
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> specialSymbols
    rest <- many (letter <|> specialSymbols <|> digit)
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
    x <- parseExpr
    return $ List $ Atom "quoted" : [x]

specialSymbols :: Parser Char
specialSymbols = oneOf "!$%&|*+-/:<=?>@^_~#"

ignoreSpaces :: Parser ()
ignoreSpaces = skipMany1 space

-- list parsing with factored out left grammar. No need for backtracking
-- in parseExpr
parseListCommon :: Parser LispVal
parseListCommon = do
    _ <- char '('
    h <- sepEndBy (skipMany space >> parseExpr) (skipMany space)
    x <- parseDotted h <|> parseNormal h
    _ <- char ')'
    return x

    where
        parseNormal :: [LispVal] -> Parser LispVal
        parseNormal h = return $ List h

        parseDotted :: [LispVal] -> Parser LispVal
        parseDotted h = do
            t <- char '.' >> ignoreSpaces >> parseExpr
            return $ DottedList h t

-- readExpr with unfactored left grammar.
-- only for comparison with readExpr
readExprUnfactored :: String -> String
readExprUnfactored inp = case parse parseExprUnfactored "lisp" inp of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

-- parseExpr with unfactored left grammar. Needs backtracking with the
-- try block as shown below
parseExprUnfactored :: Parser LispVal
parseExprUnfactored = parseAtom <|>
            parseNumber <|>
            parseString <|>
            -- parseSingleQuoted, parseListUnfactored and parseDottedListUnfactored allow recursion
            -- becase they call parseExpr
            parseSingleQuoted <|>
            do
                _ <- char '('
                -- the try allows backtracking in case the parseListUnfactored fails
                x <- try parseListUnfactored <|> parseDottedListUnfactored
                _ <- char ')'
                return x

parseListUnfactored :: Parser LispVal
parseListUnfactored = liftM List $ sepBy parseExpr (skipMany space)

-- A DottedList is the CAR-CADR type Lisp list
parseDottedListUnfactored :: Parser LispVal
parseDottedListUnfactored = do
    h <- endBy parseExpr space
    t <- char '.' >> ignoreSpaces >> parseExpr
    return $ DottedList h t
