{-# LANGUAGE NoMonomorphismRestriction #-}
module Proof where

import Control.Applicative
import Control.Monad
import Data.List

import Type
import Term

alphabet = ['a'..'z']
reserved = ["fst", "snd", "either", "curry"]
symbols = ([1..] >>= (`replicateM` alphabet)) \\ reserved

(|-) :: MonadPlus f => [(Term, Type)] -> Type -> f Term
ps |- g =
    msum (pure . fst <$> findPremise g) <|>
    msum (breakdownPremise <$> ps) <|>
    breakdownGoal g
    where
        findPremise t = filter ((== t) . snd) ps
        breakdownPremise p@(n, t) = case t of
            And t1 t2 ->
                ((apply "fst" n, t1):(apply "snd" n, t2):delete p ps) |- g
            Or t1 t2 ->
                liftA2 (\r1 r2 -> apply3 "either" (Lambda v1 r1) (Lambda v2 r2) n)
                    (((Var v1, t1):delete p ps) |- g)
                    (((Var v2, t2):delete p ps) |- g)
                where v1:v2:_ = unusedSymbols
            -- LJT:
            Function t1 t2 | n':_ <- fst <$> findPremise t1 ->
                ((Apply n n', t2):delete p ps) |- g
            Function (And t11 t12) t2 ->
                ((apply "curry" n, Function t11 (Function t12 t2)):delete p ps) |- g
            Function (Or t11 t12) t2 ->
                ((apply "afterLeft" n, Function t11 t2):(apply "afterRight" n, Function t12 t2):delete p ps) |- g
            Function t1@(Function t11 t12) t2 -> do
                r <- ((apply "afterConst" n, Function t12 t2):delete p ps) |- t1
                ((Apply n r, t2):delete p ps) |- g
            -- LJ:
            -- Function t1 t2 -> do
            --     r <- ps |- t1
            --     ((Apply n r, t2):delete p ps) |- g
            _ -> empty
        breakdownGoal g = case g of
            And g1 g2 ->
                Pair <$> ps |- g1 <*> ps |- g2
            Or g1 g2 ->
                apply "Left" <$> ps |- g1 <|> apply "Right" <$> ps |- g2
            Function g1 g2 ->
                Lambda v <$> ((Var v, g1):ps) |- g2
                where v:_ = unusedSymbols
            _ -> empty
        unusedSymbols = filter (\v -> all ((unused v) . fst) ps) symbols

prove = ([] |-)
