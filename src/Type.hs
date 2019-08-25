module Type where

import Control.Monad
import Data.Char
import Text.Read

data Type = Type String
          | And Type Type
          | Or Type Type
          | Function Type Type
          deriving (Show, Eq)

instance Read Type where
    readPrec = parens $ readType +++ readAnd +++ readOr +++ readFunction where
        readType = do
            Ident t <- lexP
            guard $ isLower (head t)
            return (Type t)
        readAnd = do
            Punc "(" <- lexP
            a        <- reset readPrec
            Punc "," <- lexP
            b        <- reset readPrec
            Punc ")" <- lexP
            return (And a b)
        readOr = prec 10 $ do
            Ident "Either" <- lexP
            a              <- step readPrec
            b              <- step readPrec
            return (Or a b)
        readFunction = prec 5 $ do
            a         <- step readPrec
            Punc "->" <- lexP
            b         <- readPrec
            return (Function a b)
    readListPrec = readListPrecDefault
