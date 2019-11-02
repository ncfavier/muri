{-# LANGUAGE NoMonomorphismRestriction #-}
module Proof where

import Control.Applicative
import Data.List
import Data.Foldable

import Type
import Term

alt = asum . fmap pure

infix 4 |-
(|-) :: (Alternative m, Monad m) => [(Type, Term)] -> Type -> m Term
ps |- g =
    alt (lookup g ps) <|>
    breakupGoalImplication g <|>
    asum (breakupPremise <$> ps) <|>
    breakupGoal g
    where
    breakupPremise p@(t, v) = case t of
        t1 :/\: t2 ->
            let (b, p, _) = bindings t
            in Let b v <$> (p ++ ps' |- g)
        t1 :\/: t2 ->
            let (b1, p1, _) = bindings t1
                (b2, p2, _)  = bindings t2
            in Case v b1 b2 <$> (p1 ++ ps' |- g) <*> (p2 ++ ps' |- g)
        t1 :->: t2 | Just v' <- lookup t1 ps' ->
            (t2, apply v v'):ps' |- g
        t11 :/\: t12 :->: t2 ->
            (t11 :->: t12 :->: t2, Var x1 :\->: Var x2 :\->: apply v (Var x1 :*: Var x2)):ps' |- g
        t11 :\/: t12 :->: t2 ->
            pl:pr:ps' |- g where
                pl = (t11 :->: t2, Var x1 :\->: apply v (Var "Left" :$: Var x1))
                pr = (t12 :->: t2, Var x2 :\->: apply v (Var "Right" :$: Var x2))
        t1@(t11 :->: t12) :->: t2 -> do
            r <- (t12 :->: t2, Var x1 :\->: apply v (Var "_" :\->: Var x1)):ps' |- t1
            (t2, apply v r):ps' |- g
        _ -> empty
        where ps' = delete p ps

    breakupGoalImplication t = case t of
        t1 :->: t2 ->
            let (b, p, _) = bindings t1
            in (b :\->:) <$> (p ++ ps |- t2)
        _ -> empty
    breakupGoal t = case t of
        t1 :/\: t2 ->
            (:*:) <$> (ps |- t1) <*> (ps |- t2)
        t1 :\/: t2 ->
            (Var "Left" :$:) <$> (ps |- t1) <|> (Var "Right" :$:) <$> (ps |- t2)
        _ -> empty

    bindings = go unusedSymbols where
        go xs (t1 :/\: t2) =
            let (b1, p1, xs')  = go xs t1
                (b2, p2, xs'') = go xs' t2
            in (b1 :*: b2, p1 ++ p2, xs'')
        go (x:xs) t = (Var x, [(t, Var x)], xs)

    unusedSymbols = [x | x <- symbols, all ((x `unusedIn`) <$> snd) ps]
    x1:x2:_ = unusedSymbols

prove = ([] |-)
