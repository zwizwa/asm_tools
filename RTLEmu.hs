{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTLEmu where

import RTL

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Map.Lazy (Map, (!), lookup, empty, insert, insertWith, fromList)
import qualified Data.Map as Map

-- For later extension
type RegNum = Int
type ConstVal = Int
type RegVal = Int

newtype M t = M { unEmu :: ReaderT RegMap (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader RegMap,
   MonadState CompState)

type RegMap = Map RegNum RegVal
type CompState = (RegNum, RegMap)

-- Primitive state manipulations
appRegNum f (n,c) = (f n, c) ; getRegNum = do (n,_) <- get ; return n
appOut    f (n,c) = (n, f c) 

data Signal = Reg Int | Val Int

-- Phantom representation wrapper
data R t = R { unR :: Signal }  

instance RTL M R where

  -- undriven signal
  signal =
    fmap (R . Reg) makeRegNum

  -- driven signals
  int c =
    return $ R $ Val c
  op1 o (R a) = do
    a' <- ref a
    return $ R $ Val $ f1 o a'
  op2 o (R a) (R b)= do
    a' <- ref a
    b' <- ref b
    return $ R $ Val $ f2 o a' b'

  -- register drive
  next (R (Reg a)) (R b) = do
    vb <- ref b
    let f _ old = error $ "Register conflict: " ++ show (a,old,vb)
    modify $ appOut $ insertWith f a vb
    
    
-- Register
ref (Val c) = return c
ref (Reg c) = asks $ (! c)

f1 o = error $ "not implemented: " ++ show o
f2 ADD = (+)
f2 o = error $ "not implemented: " ++ show o

makeRegNum = do
  n <- getRegNum
  modify $ appRegNum (+ 1)
  return  n

-- Take map as input
compile m si = so where
  (v, (_, so)) = runEmu m si

-- FIXME: How to allow compile to provide output apart from register
-- state out?  Tried putting v out, but this has a ref wrapping.  Is
-- there a generic way to unpack all refs?  Maybe this just needs to
-- be done at the calling site?

-- To compute initial value, run it once with register output tied to
-- input.  If no evaluation happens this can compute the register
-- count, from which we create an initial map.
init m = fromList $ [(r,0) | r <- [0..n-1]] where
  (_, (n, s)) = runEmu m s

runEmu m i = runState (runReaderT (unEmu m) i) (0, empty)

