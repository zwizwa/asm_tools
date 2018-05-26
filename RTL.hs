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
data SType = SInt NbBits | SInt'
type NbBits = Int

-- Semantics of signals:
-- 1) have exactly one driver
-- 2) is a wire if driven from combinatorial context
-- 3) is a register if driven from sequential context

-- Explicit assignment commands are used to distinguish between
-- combinatorial and sequential assignment.  Typical HDLs use a
-- context in which assignment changes semantics.

class Monad m => RTL m r where

  -- Signal creaton
  signal  :: SType -> m (r S)    -- Undriven
  stype   :: r S -> m (SType)
  int     :: Int -> m (r S)      -- Driven by constant

  -- Drive
  next    :: r S -> r S -> m ()  -- Register update equation

  -- Combinatorial operations all create driven intermediate signals
  -- to make them fit better in a monadic language.
  op2 :: Op2 -> r S -> r S -> m (r S)
  op1 :: Op1 -> r S -> m (r S)


-- Note that combinatorial drive is a code smell.  It is typically a
-- good idea to register the outputs of a module, meaing that all
-- combinatorial signals are generated implicitly through opx.

data Op2 = ADD | AND | XOR | SLL | SLR deriving Show
data Op1 = INV deriving Show

add :: forall m r. RTL m r => r S -> r S -> m (r S)
add = op2 ADD

band :: forall m r. RTL m r => r S -> r S -> m (r S)
band = op2 AND

bxor :: forall m r. RTL m r => r S -> r S -> m (r S)
bxor = op2 XOR

sll :: forall m r. RTL m r => r S -> r S -> m (r S)
sll = op2 SLL

slr :: forall m r. RTL m r => r S -> r S -> m (r S)
slr = op2 SLR

inv :: forall m r. RTL m r => r S -> m (r S)
inv = op1 INV



