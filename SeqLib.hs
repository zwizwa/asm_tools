{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SeqLib where
import Seq

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
  if' e s0 s     -- conditional reset
  
-- Bound to register
sync :: Seq m r => SType -> r S -> m (r S)
sync t i = do
  s0 <- constant t
  reg t $ sync' s0 i

