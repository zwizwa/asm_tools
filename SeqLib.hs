{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SeqLib where
import Seq
import Control.Monad


-- High level register feedback operator.  Bundling 'signal' with
-- 'next' avoids the creation of undriven or multiply-driven signals.
-- This is preferred over using the low-level primitives directly.

-- Close register loop, return register output and input.
reg' :: Seq m r => SType -> (r S -> m (r S)) -> m (r S, r S)
reg' t f = do
  r <- signal t
  r' <- f r
  next r r'
  return (r, r')

-- Same for multiple registers contained in meta-level lists.
-- Generalize list to functor?  List might be enough.
regs' :: Seq m r => [SType] -> ([r S] -> m [r S]) -> m ([r S], [r S])
regs' ts f = do
  rs <- sequence $ map signal ts
  rs' <- f rs
  sequence_ $ zipWith next rs rs'
  return (rs, rs')

-- Same, return just the outputs.
reg  t  f = reg'  t  f >>= return . snd
regs ts f = regs' ts f >>= return . snd




-- Some simple building blocks

inc :: Seq m r => r S -> m (r S)
inc c = int 1 >>= add c

counter :: Seq m r => SType -> m (r S)
counter t = reg t inc

delay :: Seq m r => r S -> m (r S)
delay x = do
  t <- stype x
  reg t $ \_ -> return x

edge d = do
  d0 <- delay d
  d `bxor` d0

bit b = do
  int 1 >>= band b


int :: forall m r. Seq m r => Int -> m (r S)
int v = constant (SInt Nothing v)


-- A test of completeness is to implement a clock synchronizer.
-- Simplify it to power-of-two division.

-- Combinatorial part
sync' :: forall m r. Seq m r => r S -> r S -> r S -> m (r S)
sync' s0 i s = do
  e  <- edge i   -- edge detector on input
  s' <- inc s    -- free running counter
  if' e s0 s'    -- conditional reset
  
-- Bound to register
sync :: Seq m r => SType -> r S -> m (r S)
sync t i = do
  s0 <- constant t
  reg t $ sync' s0 i


