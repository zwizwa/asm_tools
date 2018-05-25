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

type Sig = Int

data Driver = Comb2 Op2 Sig Sig
            | Comb1 Op1 Sig
            | Delay Sig
            | Const Int
  deriving Show

newtype Gen t = Gen { unGen :: WriterT [W] (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadWriter [W],
   MonadState CompState)

type W = Char -- not really needed?

type SignalMap = Map Sig Driver
type CompState = (Int, SignalMap)

-- Primitive state manipulations
appSignal f (n,c) = (f n, c)
appComb   f (n,c) = (n, f c) ; getSignal = do (n,_) <- get ; return n

instance RTL Gen where
  
  op1 o a = do
    sa <- ref a
    driven $ Comb1 o sa

  op2 o a b = do
    sa <- ref a
    sb <- ref b
    driven $ Comb2 o sa sb

  set (S a) (S b) =
    driveSignal a $ Delay b

signal = do
  n <- getSignal
  modify $ appSignal $ (+ 1)
  return $ S n

-- Constants are represented as drivers.
ref (S n) =
  return n
ref (L i) = do
  S n <- driven $ Const i
  return n

driveSignal n c = do
  modify $ appComb $ insert n c

driven c = do
  s@(S n) <- signal
  driveSignal n c
  return s



compile m = dict where
  ((v, w), s) = runState (runWriterT $ unGen m) (0, empty)
  (_, dict) = s
