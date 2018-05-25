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

-- FIXME: currently there are no contexts, but there are different
-- connectivity primitives.

class Monad m => RTL m r where

  -- Signal creaton
  signal  :: m (r Sig)               -- Undriven
  lit     :: Int -> m (r Sig)        -- Driven by constant

  -- Drive
  connect :: r Sig -> r Sig -> m ()  -- Combinatorial connect
  delay   :: r Sig -> r Sig -> m ()  -- Register write

  -- Combinatorial operations all create intermediate signals to make
  -- them fit better in a monadic language.
  op2 :: Op2 -> r Sig -> r Sig -> m (r Sig)
  op1 :: Op1 -> r Sig -> m (r Sig)


data Op2 = ADD deriving Show
data Op1 = INV deriving Show

add :: forall m r. RTL m r => r Sig -> r Sig -> m (r Sig)
add = op2 ADD

inv :: forall m r. RTL m r => r Sig -> m (r Sig)
inv = op1 INV

