module Type where

import Control.Monad
import Data.Char
import Text.Read

infixl 3 :/\:
infixl 2 :\/:
infixr 0 :->:
data Type = TVar String
          | TUnit
          | Type :/\: Type
          | Void
          | Type :\/: Type
          | Type :->: Type
          deriving (Show, Eq)

instance Read Type where
    readPrec = parens $ readType +++ readUnit +++ readAnd +++ readVoid +++ readOr +++ readFunction where
        readType = do
            Ident t <- lexP
            guard $ isLower (head t)
            return (TVar t)
        readUnit = do
            Punc "(" <- lexP
            Punc ")" <- lexP
            return TUnit
        readAnd = do
            Punc "(" <- lexP
            a        <- reset readPrec
            Punc "," <- lexP
            b        <- reset readPrec
            Punc ")" <- lexP
            return (a :/\: b)
        readVoid = do
            Ident "Void" <- lexP
            return Void
        readOr = prec 10 $ do
            Ident "Either" <- lexP
            a              <- step readPrec
            b              <- step readPrec
            return (a :\/: b)
        readFunction = prec 5 $ do
            a         <- step readPrec
            Punc "->" <- lexP
            b         <- readPrec
            return (a :->: b)
    readListPrec = readListPrecDefault
