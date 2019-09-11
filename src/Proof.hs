{-# LANGUAGE NoMonomorphismRestriction #-}
module Proof where

import Control.Applicative
import Data.List
import Data.Foldable

import Type
import Term

liftMaybe = maybe empty pure

infix 4 |-
ps |- g = liftMaybe (lookup g ps) <|> asum (breakupPremise <$> ps) <|> breakupGoal g where
    breakupPremise p@(t, v) = case t of
        t1 :/\: t2 ->
            Unpair v x1 x2 <$> ((t1, Var x1):(t2, Var x2):ps' |- g)
        t1 :\/: t2 ->
            Uneither v x1 x2 <$> ((t1, Var x1):ps' |- g) <*> ((t2, Var x2):ps' |- g)
        t1 :->: t2 | Just v' <- lookup t1 ps ->
            (t2, v :$: v'):ps' |- g
        t11 :/\: t12 :->: t2 ->
            (t11 :->: t12 :->: t2, x1 :\->: x2 :\->: v :$: (Var x1 :*: Var x2)):ps' |- g
        t11 :\/: t12 :->: t2 ->
            pl:pr:ps' |- g where
                pl = (t11 :->: t2, x1 :\->: v :$: (Var "Left" :$: Var x1))
                pr = (t12 :->: t2, x2 :\->: v :$: (Var "Right" :$: Var x2))
        t1@(t11 :->: t12) :->: t2 -> do
            r <- (t12 :->: t2, x1 :\->: v :$: ("_" :\->: Var x1)):ps' |- t1
            (t2, v :$: r):ps' |- g
        _ -> empty
        where ps' = delete p ps

    breakupGoal t = case t of
        t1 :/\: t2 -> (:*:) <$> (ps |- t1) <*> (ps |- t2)
        t1 :\/: t2 -> (Var "Left" :$:) <$> (ps |- t1) <|> (Var "Right" :$:) <$> (ps |- t2)
        t1 :->: t2 -> (x1 :\->:) <$> ((t1, Var x1):ps |- t2)
        _ -> empty

    x1:x2:_ = unusedSymbols ps

prove = ([] |-)
