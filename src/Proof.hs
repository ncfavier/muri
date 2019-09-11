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
        And t1 t2 ->
            Unpair v x1 x2 <$> ((t1, Var x1):(t2, Var x2):ps' |- g)
        Or t1 t2 ->
            Uneither v x1 x2 <$> ((t1, Var x1):ps' |- g) <*> ((t2, Var x2):ps' |- g)
        Function t1 t2 | Just v' <- lookup t1 ps ->
            (t2, Apply v v'):ps' |- g
        Function (And t11 t12) t2 ->
            (Function t11 (Function t12 t2), Lambda x1 (Lambda x2 (Apply v (Pair (Var x1) (Var x2))))):ps' |- g
        Function (Or t11 t12) t2 ->
            pl:pr:ps' |- g where
                pl = (Function t11 t2, Lambda x1 (Apply v (applyLeft (Var x1))))
                pr = (Function t12 t2, Lambda x2 (Apply v (applyRight (Var x2))))
        Function t1@(Function t11 t12) t2 -> do
            r <- (Function t12 t2, Lambda x1 (Apply v (Lambda "_" (Var x1)))):ps' |- t1
            (t2, Apply v r):ps' |- g
        _ -> empty
        where ps' = delete p ps

    breakupGoal t = case t of
        And t1 t2 -> Pair <$> (ps |- t1) <*> (ps |- t2)
        Or t1 t2 -> applyLeft <$> (ps |- t1) <|> applyRight <$> (ps |- t2)
        Function t1 t2 -> Lambda x1 <$> ((t1, Var x1):ps |- t2)
        _ -> empty

    x1:x2:_ = unusedSymbols ps

prove = ([] |-)
