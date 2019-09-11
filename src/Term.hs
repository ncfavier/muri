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
    showsPrec d (Var v) = showString v
    showsPrec d (Pair a b) =
        showChar '(' . showsPrec 0 a . showString ", " . showsPrec 0 b . showChar ')'
    showsPrec d (Unpair p v1 v2 e) = showParen (d > prec) $
        showString ("let (" ++ v1 ++ ", " ++ v2 ++ ") = ") . showsPrec prec p . showString " in " . showsPrec prec e
        where prec = 0
    showsPrec d (Uneither e v1 v2 l r) = showParen (d > prec) $
        showString "case " . showsPrec prec e . showString (" of { Left " ++ v1 ++ " -> ") . showsPrec prec l . showString ("; Right " ++ v2 ++ " -> ") . showsPrec prec r . showString " }"
        where prec = 0
    showsPrec d (Apply f x) = showParen (d > prec) $
        showsPrec prec f . showChar ' ' . showsPrec (prec + 1) x
        where prec = 10
    showsPrec d (Lambda v x) = showParen (d > prec) $
        showChar '\\' . showString v . showString " -> " . showsPrec prec x
        where prec = 0

symbols = [1..] >>= (`replicateM` ['a'..'z'])

unused v (Var v') = v /= v'
unused v (Pair t1 t2) = unused v t1 && unused v t2
unused v (Apply t1 t2) = unused v t1 && unused v t2
unused v (Lambda v' t) = v /= v' && unused v t

applyLeft  = Apply (Var "Left")
applyRight = Apply (Var "Right")
