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

newtype Gen t = Gen { unGen :: WriterT [W] (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadWriter [W],
   MonadState CompState)

type W = Char -- not really needed?

type SignalMap = Map SigNum Driver
type CompState = (SigNum, SignalMap)

-- Primitive state manipulations
appSignal f (n,c) = (f n, c)
appComb   f (n,c) = (n, f c) ; getSignal = do (n,_) <- get ; return n

data Signal = Signal Int

-- Phantom representation wrapper
data Rep t = Rep { unRep :: Signal }  

instance RTL Gen Rep where

  -- undriven signal
  signal = do
    s <- makeSignal
    return $ Rep s

  -- driven signals
  op1 o (Rep (Signal a)) = do
    s <- driven $ Comb1 o a
    return $ Rep s
  op2 o (Rep (Signal a)) (Rep (Signal b)) = do
    s <- driven $ Comb2 o a b
    return $ Rep s
  lit c = do
    s <- driven $ Const c
    return $ Rep s

  -- combinatorial drive
  connect (Rep (Signal dst)) (Rep (Signal src)) =
    driveSignal dst $ Connect src

  -- register drive
  delay (Rep (Signal dst)) (Rep (Signal src)) =
    driveSignal dst $ Delay src

makeSignal = do
  n <- getSignal
  modify $ appSignal (+ 1)
  return $ Signal n

driveSignal n c = do
  -- FIXME: error when already driven
  modify $ appComb $ insert n c

driven c = do
  s@(Signal n) <- makeSignal
  driveSignal n c
  return s

compile m = dict where
  ((v, w), s) = runState (runWriterT $ unGen m) (0, empty)
  (_, dict) = s
