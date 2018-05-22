{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Pru where


class Monad m => Pru m where
  label :: m Label
  block :: Label -> m ()
  jmp :: Label -> m ()
  op2 :: Opc2 -> Op -> Op -> m ()

data Op = OpR R           deriving (Show, Eq, Ord)
data Opc2 = Mov           deriving Show
data Ins = Op2 Opc2 Op Op
         | Jmp Label      
         | Label Label    deriving Show
type Label = Int

type R = Int


r10 = OpR 10
r11 = OpR 11

mov :: Pru m => Op -> Op -> m ()
mov = op2 Mov

-- As a convenience.  The explicit dereference is quite annoying when
-- the reference producer doesn't have any side effects.
o2 f a b = do
  a' <- a
  b' <- b
  f a' b'

  

