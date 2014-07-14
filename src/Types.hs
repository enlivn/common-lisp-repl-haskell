module Types where

-- An Atom is a letter or symbol followed by any number of letters, digits,
-- or symbols
-- A DottedList is the CAR-CADR type Lisp list
data LispVal =  Atom String |
                Number Integer |
                Bool Bool |
                String String |
                List [LispVal] |
                DottedList [LispVal] LispVal
                deriving Show
