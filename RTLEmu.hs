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

data Signal = Reg Size Int | Val Size Int
type Size = Maybe Int

-- Phantom representation wrapper
data R t = R { unR :: Signal }  

instance RTL M R where

  -- undriven signal
  signal (SInt bits) = fmap (R . Reg bits) $ makeRegNum
  stype (R a) = do sz <- bits a ;  return $ SInt sz

  -- driven signals
  int (SInt sz) c =
    return $ R $ Val sz c
  op1 o (R a) = do
    (sza, va) <- ref' a
    return $ R $ truncVal sza $ f1 o va
  op2 o (R a) (R b)= do
    (sza,va) <- ref' a
    (szb,vb) <- ref' b
    return $ R $ truncVal (sz sza szb) $ f2 o va vb

  -- register drive
  next (R (Reg sz a)) (R b) = do
    vb <- ref b
    let ifConflict _ old = error $ "Register conflict: " ++ show (a,old,vb)
    modify $ appOut $ insertWith ifConflict a vb
    
    
-- Register
ref' (Val b c) = return (b,c)
ref' (Reg b c) = do v <- asks (! c) ; return (b, v)
ref  = (fmap snd) . ref'
bits = (fmap fst) . ref'

sz Nothing a = a
sz a Nothing = a
sz (Just a) (Just b) = Just $ max a b

truncVal Nothing v     = Val Nothing v
truncVal sz@(Just b) v = Val sz $ v .&. mask b

mask b = (shiftL 1 b) - 1

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
  deref (R (Val _ v)) = v
  deref (R (Reg _ r)) = so ! r

-- To compute initial value, run it once with register output tied to
-- input.  This won't diverge because the output is never evaluated,
-- but does produce a register count. From that we can create an
-- initial map.
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
