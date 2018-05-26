{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTLNet where

import RTL

import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map, (!), lookup, empty, insert, insertWith)
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

newtype M t = M { unNet :: State CompState t } deriving
  (Functor, Applicative, Monad,
   MonadState CompState)

type SignalMap = Map SigNum Driver
type CompState = (SigNum, SignalMap)

-- Primitive state manipulations
appSignal f (n,c) = (f n, c)
appComb   f (n,c) = (n, f c) ; getSignal = do (n,_) <- get ; return n

data Signal = Sig Int

-- Phantom representation wrapper
data R t = R { unR :: Signal }  

instance RTL M R where

  -- undriven signal
  signal                        = fmap R makeSignal

  -- driven signals
  op1 o (R (Sig a))             = fmap R $ driven $ Comb1 o a
  op2 o (R (Sig a)) (R (Sig b)) = fmap R $ driven $ Comb2 o a b
  int c                         = fmap R $ driven $ Const c

  -- combinatorial drive
  connect (R (Sig dst)) (R (Sig src)) =
    driveSignal dst $ Connect src

  -- register drive
  next (R (Sig dst)) (R (Sig src)) =
    driveSignal dst $ Delay src

makeSignal = do
  n <- getSignal
  modify $ appSignal (+ 1)
  return $ Sig n

driveSignal n c = do
  let f _ old_c = error $ "Signal driven twice: " ++ show (n,old_c,c)
  modify $ appComb $ insertWith f n c

driven c = do
  s@(Sig n) <- makeSignal
  driveSignal n c
  return s

compile m = signals where
  (_, (_, signals)) = runState (unNet m) (0, empty)
