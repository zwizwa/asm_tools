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

newtype M t = M { unEmu :: ReaderT RegMap (WriterT [W] (State CompState)) t } deriving
  (Functor, Applicative, Monad,
   MonadWriter [W],
   MonadReader RegMap,
   MonadState CompState)

type W = Char -- not really needed?

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
  signal            = fmap (R . Reg) makeRegNum

  -- driven signals
  op1 o (R a)       = fmap (R . Val) $ do ; va <- ref a ; return $ f1 o va
  op2 o (R a) (R b) = fmap (R . Val) $ do ; va <- ref a ; vb <- ref b ; return $ f2 o va vb
  int c             = return $ R $ Val c

  -- register drive
  next (R (Reg a)) (R b) = do
    vb <- ref b
    modify $ appOut $ insert a vb
    
    
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
compile m = snd . f where
  f i = s where
    ((v, w), s) = runEmu m i

-- To compute initial value, run it once with output tied to input.
-- If no evaluation happens this can compute the register count, from
-- which we create an initial map.
init m = fromList $ [(r,0) | r <- [0..n-1]] where
  ((v, w), (n, o)) = runEmu m o

runEmu m i = runState (runWriterT (runReaderT (unEmu m) i)) (0, empty)
