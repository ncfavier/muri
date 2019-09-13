module Term where

import Control.Monad

infixl 3 :*:
infixl 9 :$:
infixr 0 :\->:
data Term = Var String
          | Term :*: Term
          | Term :$: Term
          | Term :\->: Term
          | Let Term Term Term
          | Case Term Term Term Term Term
          deriving (Read, Eq)

instance Show Term where
    showsPrec d (Var x) = showString x
    showsPrec d (l :*: r) =
        showChar '(' . showsPrec 0 l . showString ", " . showsPrec 0 r . showChar ')'
    showsPrec d (v :$: v') = showParen (d > prec) $
        showsPrec prec v . showChar ' ' . showsPrec (prec + 1) v'
        where prec = 10
    showsPrec d (x :\->: e) = showParen (d > prec) $
        showChar '\\' . showsPrec prec x . showString " -> " . showsPrec prec e
        where prec = 0
    showsPrec d (Let b v e) = showParen (d > prec) $
        showString "let " . showsPrec prec b . showString " = " . showsPrec prec v . showString " in " . showsPrec prec e
        where prec = 0
    showsPrec d (Case v b1 b2 e1 e2) = showParen (d > prec) $
        showString "case " . showsPrec prec v . showString " of { Left " . showsPrec 11 b1 . showString " -> " . showsPrec prec e1 . showString "; Right " . showsPrec 11 b2 . showString " -> " . showsPrec prec e2 . showString " }"
        where prec = 0

symbols = [1..] >>= (`replicateM` ['a'..'z'])

x `unusedIn` Var x' = x /= x'
x `unusedIn` (t1 :*: t2) = x `unusedIn` t1 && x `unusedIn` t2
x `unusedIn` (t1 :$: t2) = x `unusedIn` t1 && x `unusedIn` t2
x `unusedIn` (x' :\->: t) = x `unusedIn` x' && x `unusedIn` t

apply (Var x :\->: e) v = substitute x v e
apply a b = a :$: b

substitute x v e = case e of
    Var y
        | y == x -> v
        | otherwise -> e
    a :*: b -> substitute x v a :*: substitute x v b
    a :$: b -> substitute x v a :$: substitute x v b
    y :\->: e'
        | y == Var x -> e'
        | otherwise -> y :\->: substitute x v e'
