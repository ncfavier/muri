{-# LANGUAGE NoMonomorphismRestriction #-}
module Proof where

import Control.Applicative
import Data.List
import Data.Foldable

import Type
import Term

liftMaybe = maybe empty pure

infix 4 |-
(|-) :: (Alternative m, Monad m) => [(Type, Term)] -> Type -> m Term
ps |- g = liftMaybe (lookup g ps) <|> asum (breakupPremise <$> ps) <|> breakupGoal g where
    breakupPremise p@(t, v) = case t of
        t1 :/\: t2 ->
            let (b, p, _) = bindings t unusedSymbols
            in Let b v <$> (p ++ ps' |- g)
        t1 :\/: t2 ->
            let (b1, p1, xs) = bindings t1 unusedSymbols
                (b2, p2, _)  = bindings t2 xs
            in Case v b1 b2 <$> (p1 ++ ps' |- g) <*> (p2 ++ ps' |- g)
        t1 :->: t2 | Just v' <- lookup t1 ps' ->
            (t2, v :$: v'):ps' |- g
        t11 :/\: t12 :->: t2 ->
            (t11 :->: t12 :->: t2, Var x1 :\->: Var x2 :\->: v :$: (Var x1 :*: Var x2)):ps' |- g
        t11 :\/: t12 :->: t2 ->
            pl:pr:ps' |- g where
                pl = (t11 :->: t2, Var x1 :\->: v :$: (Var "Left" :$: Var x1))
                pr = (t12 :->: t2, Var x2 :\->: v :$: (Var "Right" :$: Var x2))
        t1@(t11 :->: t12) :->: t2 -> do
            r <- (t12 :->: t2, Var x1 :\->: v :$: (Var "_" :\->: Var x1)):ps' |- t1
            (t2, v :$: r):ps' |- g
        _ -> empty
        where ps' = delete p ps

    breakupGoal t = case t of
        t1 :/\: t2 ->
            (:*:) <$> (ps |- t1) <*> (ps |- t2)
        t1 :\/: t2 ->
            (Var "Left" :$:) <$> (ps |- t1) <|> (Var "Right" :$:) <$> (ps |- t2)
        t1 :->: t2 ->
            let (b, p, _) = bindings t1 unusedSymbols
            in (b :\->:) <$> (p ++ ps |- t2)
        _ -> empty

    bindings (t1 :/\: t2) xs =
        let (b1, p1, xs')  = bindings t1 xs
            (b2, p2, xs'') = bindings t2 xs'
        in (b1 :*: b2, p1 ++ p2, xs'')
    bindings t (x:xs) = (Var x, [(t, Var x)], xs)

    unusedSymbols = [x | x <- symbols, all ((x `unusedIn`) <$> snd) ps]
    x1:x2:_ = unusedSymbols

prove = ([] |-)
