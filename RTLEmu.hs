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
import Data.Bits


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

f1 INV = complement

f2 ADD = (+)
f2 AND = (.&.)
f2 XOR = xor
f2 SLL = shiftL
f2 SLR = shiftR

makeRegNum = do
  n <- getRegNum
  modify $ appRegNum (+ 1)
  return n

runEmu m i = runState (runReaderT (unEmu m) i) (0, empty)

-- Take map as input
compileUpdate m si = so where
  (_, (_, so)) = runEmu m si

-- Same, but require a signal list as monadic value.
compileUpdate' :: M [R S] -> RegMap -> (RegMap, [Int])
compileUpdate' m si = (so, o) where
  (sigs, (_, so)) = runEmu m si
  o = map deref sigs
  deref (R (Val v)) = v
  deref (R (Reg r)) = so ! r

-- To compute initial value, run it once with register output tied to
-- input.  This won't diverge because the output is never evaluated,
-- but does produce a register count, from which we create an initial
-- map.
compileInit m = fromList $ [(r,0) | r <- [0..n-1]] where
  (_, (n, s)) = runEmu m s

-- Final product gives enough information to produce an infinite sequence.
compile  m = (compileInit m, compileUpdate  m)
compile' m = (compileInit m, compileUpdate' m)

-- Traces will not expose state
trace :: M [R S] -> [[Int]]
trace m = t s0 where
  (s0, f) = compile' m
  t s = o : t s' where (s', o) = f s
