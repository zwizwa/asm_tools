{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTL where

-- Abstract tag for signal representation.
data Sig = Sig

-- Semantics of signals:
-- 1) have exactly one driver
-- 2) is a wire if driven from combinatorial context
-- 3) is a register if driven from sequential context

-- Explicit assignment commands are used to distinguish between
-- combinatorial and sequential assignment.  Typical HDLs use a
-- context in which assignment changes semantics.

class Monad m => RTL m r where

  -- Signal creaton
  signal  :: m (r Sig)               -- Undriven
  int     :: Int -> m (r Sig)        -- Driven by constant

  -- Drive
  connect :: r Sig -> r Sig -> m ()  -- Combinatorial connect
  next    :: r Sig -> r Sig -> m ()  -- Register update equation

  -- Combinatorial operations all create intermediate signals to make
  -- them fit better in a monadic language.
  op2 :: Op2 -> r Sig -> r Sig -> m (r Sig)
  op1 :: Op1 -> r Sig -> m (r Sig)

-- Note that combinatorial drive is a code smell.  It is typically a
-- good idea to register the outputs of a module, meaing that all
-- combinatorial signals are generated implicitly through opx.

data Op2 = ADD deriving Show
data Op1 = INV deriving Show

add :: forall m r. RTL m r => r Sig -> r Sig -> m (r Sig)
add = op2 ADD

inv :: forall m r. RTL m r => r Sig -> m (r Sig)
inv = op1 INV

