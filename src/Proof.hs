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
    breakupPremise p@(t, n) = case t of
        And t1 t2 ->
            Unpair n v1 v2 <$> ((t1, Var v1):(t2, Var v2):ps' |- g)
        Or t1 t2 ->
            Uneither n v1 v2 <$> ((t1, Var v1):ps' |- g) <*> ((t2, Var v2):ps' |- g)
        Function t1 t2 | Just n' <- lookup t1 ps ->
            (t2, Apply n n'):ps' |- g
        Function (And t11 t12) t2 ->
            (Function t11 (Function t12 t2), Lambda v1 (Lambda v2 (Apply n (Pair (Var v1) (Var v2))))):ps' |- g
        Function (Or t11 t12) t2 ->
            pl:pr:ps' |- g where
                pl = (Function t11 t2, Lambda v1 (Apply n (applyLeft (Var v1))))
                pr = (Function t12 t2, Lambda v2 (Apply n (applyRight (Var v2))))
        Function t1@(Function t11 t12) t2 -> do
            r <- (Function t12 t2, Lambda v1 (Apply n (Lambda "_" (Var v1)))):ps' |- t1
            (t2, Apply n r):ps' |- g
        _ -> empty
        where ps' = delete p ps

    breakupGoal g = case g of
        And g1 g2 -> Pair <$> (ps |- g1) <*> (ps |- g2)
        Or g1 g2 -> applyLeft <$> (ps |- g1) <|> applyRight <$> (ps |- g2)
        Function g1 g2 -> Lambda v1 <$> ((g1, Var v1):ps |- g2)
        _ -> empty

    v1:v2:_ = unusedSymbols ps

prove = ([] |-)
