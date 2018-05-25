{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTLGen where

import RTL

import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map, (!), lookup, empty, insert, fromList, adjust)
import qualified Data.Map as Map

-- For later extension
type SigNum = Int
type ConstVal = Int

data Driver = Comb2 Op2 SigNum SigNum
            | Comb1 Op1 SigNum
            | Delay SigNum
            | Connect SigNum
            | Const ConstVal
  deriving Show

newtype M t = M { unGen :: WriterT [W] (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadWriter [W],
   MonadState CompState)

type W = Char -- not really needed?

type SignalMap = Map SigNum Driver
type CompState = (SigNum, SignalMap)

-- Primitive state manipulations
appSignal f (n,c) = (f n, c)
appComb   f (n,c) = (n, f c) ; getSignal = do (n,_) <- get ; return n

data Signal = S Int

-- Phantom representation wrapper
data R t = R { unR :: Signal }  

instance RTL M R where

  -- undriven signal
  signal                    = fmap R makeSignal

  -- driven signals
  op1 o (R (S a))           = fmap R $ driven $ Comb1 o a
  op2 o (R (S a)) (R (S b)) = fmap R $ driven $ Comb2 o a b
  lit c                     = fmap R $ driven $ Const c

  -- combinatorial drive
  connect (R (S dst)) (R (S src)) =
    driveSignal dst $ Connect src

  -- register drive
  next (R (S dst)) (R (S src)) =
    driveSignal dst $ Delay src

makeSignal = do
  n <- getSignal
  modify $ appSignal (+ 1)
  return $ S n

driveSignal n c = do
  -- FIXME: error when already driven
  modify $ appComb $ insert n c

driven c = do
  s@(S n) <- makeSignal
  driveSignal n c
  return s

compile m = dict where
  ((v, w), s) = runState (runWriterT $ unGen m) (0, empty)
  (_, dict) = s
