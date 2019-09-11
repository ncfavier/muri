module Term where

import Control.Monad

data Term = Var String
          | Pair Term Term
          | Unpair Term String String Term
          | Uneither Term String String Term Term
          | Apply Term Term
          | Lambda String Term
          deriving (Read, Eq)

instance Show Term where
    showsPrec d (Var x) = showString x
    showsPrec d (Pair l r) =
        showChar '(' . showsPrec 0 l . showString ", " . showsPrec 0 r . showChar ')'
    showsPrec d (Unpair p x1 x2 e) = showParen (d > prec) $
        showString ("let (" ++ x1 ++ ", " ++ x2 ++ ") = ") . showsPrec prec p . showString " in " . showsPrec prec e
        where prec = 0
    showsPrec d (Uneither e x1 x2 l r) = showParen (d > prec) $
        showString "case " . showsPrec prec e . showString (" of { Left " ++ x1 ++ " -> ") . showsPrec prec l . showString ("; Right " ++ x2 ++ " -> ") . showsPrec prec r . showString " }"
        where prec = 0
    showsPrec d (Apply v v') = showParen (d > prec) $
        showsPrec prec v . showChar ' ' . showsPrec (prec + 1) v'
        where prec = 10
    showsPrec d (Lambda x v) = showParen (d > prec) $
        showChar '\\' . showString x . showString " -> " . showsPrec prec v
        where prec = 0

symbols = [1..] >>= (`replicateM` ['a'..'z'])

x `unusedIn` Var x' = x /= x'
x `unusedIn` Pair t1 t2 = x `unusedIn` t1 && x `unusedIn` t2
x `unusedIn` Apply t1 t2 = x `unusedIn` t1 && x `unusedIn` t2
x `unusedIn` Lambda x' t = x /= x' && x `unusedIn` t

unusedSymbols ps = [x | x <- symbols, all ((x `unusedIn`) <$> snd) ps]

applyLeft  = Apply (Var "Left")
applyRight = Apply (Var "Right")
