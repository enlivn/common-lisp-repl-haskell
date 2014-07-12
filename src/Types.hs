module Types where

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
