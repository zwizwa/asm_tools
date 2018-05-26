{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTL where

-- Abstract tag for signal representation.
data S = S

-- Semantics of signals:
-- 1) have exactly one driver
-- 2) is a wire if driven from combinatorial context
-- 3) is a register if driven from sequential context

-- Explicit assignment commands are used to distinguish between
-- combinatorial and sequential assignment.  Typical HDLs use a
-- context in which assignment changes semantics.

class Monad m => RTL m r where

  -- Signal creaton
  signal  :: m (r S)             -- Undriven
  int     :: Int -> m (r S)      -- Driven by constant

  -- Drive
  connect :: r S -> r S -> m ()  -- Combinatorial connect
  next    :: r S -> r S -> m ()  -- Register update equation

  -- Combinatorial operations all create intermediate signals to make
  -- them fit better in a monadic language.
  op2 :: Op2 -> r S -> r S -> m (r S)
  op1 :: Op1 -> r S -> m (r S)

  connect _ _ = error $ "connect not implemented"

-- Note that combinatorial drive is a code smell.  It is typically a
-- good idea to register the outputs of a module, meaing that all
-- combinatorial signals are generated implicitly through opx.

data Op2 = ADD | XOR | SLL deriving Show
data Op1 = INV deriving Show

add :: forall m r. RTL m r => r S -> r S -> m (r S)
add = op2 ADD

xor :: forall m r. RTL m r => r S -> r S -> m (r S)
xor = op2 XOR

sll :: forall m r. RTL m r => r S -> r S -> m (r S)
sll = op2 SLL

inv :: forall m r. RTL m r => r S -> m (r S)
inv = op1 INV


