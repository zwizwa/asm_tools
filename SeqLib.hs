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


-- Special fixReg case: single register, with register as output
reg :: Seq m r => SType -> (r S -> m (r S)) -> m (r S)
reg t f = do fixReg [t] $ \[r] -> do r' <- f r ; return ([r'], r)


-- Some simple building blocks

inc :: Seq m r => r S -> m (r S)
inc c = add c (int 1)

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
  band b (int 1)


int :: forall m r. Seq m r => Int -> r S
int v = constant (SInt Nothing v)


-- A test of completeness is to implement a clock synchronizer.
-- Simplify it to power-of-two division.

-- Combinatorial part
sync' :: forall m r. Seq m r => r S -> r S -> r S -> m (r S)
sync' s0 i s = do
  e  <- edge i   -- edge detector on input
  s' <- inc s    -- default is free running counter
  if' e s0 s'    -- conditional reset on edge
  
-- Bound to register
sync :: Seq m r => SType -> r S -> m (r S)
sync t i = do
  reg t $ sync' (constant t) i

-- Note: we only support the combinatorial (dual-clause) if.
case' :: Seq m r => [(m (r S), m (r S))] -> m (r S) -> m (r S)
case' [] dflt = dflt
case' ((cond, whenTrue):cases) dflt = do
  c <- cond
  t <- whenTrue
  f <- case' cases dflt
  if' c t f
  

  
