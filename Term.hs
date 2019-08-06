module Term where

data Term = Var String
          | Pair Term Term
          | Apply Term Term
          | Lambda String Term
          deriving (Read, Eq)

instance Show Term where
    showsPrec d (Var v) = showString v
    showsPrec d (Pair a b) =
        showChar '(' . showsPrec 0 a . showString ", " . showsPrec 0 b . showChar ')'
    showsPrec d (Apply f x) = showParen (d > prec) $
        showsPrec prec f . showChar ' ' . showsPrec (prec + 1) x
        where prec = 10
    showsPrec d (Lambda v x) = showParen (d > prec) $
        showChar '\\' . showString v . showString " -> " . showsPrec prec x
        where prec = 0

apply f t = Apply (Var f) t
apply2 f t1 t2 = Apply (Apply (Var f) t1) t2
apply3 f t1 t2 t3 = Apply (Apply (Apply (Var f) t1) t2) t3

unused :: String -> Term -> Bool
unused v (Var v') = v /= v'
unused v (Pair t1 t2) = unused v t1 && unused v t2
unused v (Apply t1 t2) = unused v t1 && unused v t2
unused v (Lambda v' t) = v /= v' && unused v t
