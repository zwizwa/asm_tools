-- An eDSL for sequential logic.

-- Note that this is not a typical HDL.

-- It helps to think of HDLs as discrete event simulation languages,
-- with logic synthesis tagged on.  We don't have that limitation and
-- intentionally limit ourselves to description of sequential logic
-- circuits.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Seq where

-- Abstract tag for signal representation.
data S = S
data SType = SInt (Maybe NbBits) InitVal
type NbBits = Int
type InitVal = Int

-- Typical HDL semantics of signals:
-- 1) have exactly one driver
-- 2) is a wire if driven in combinatorial context
-- 3) is a register if driven in sequential context

-- We do not use contexts as is typical in HDLs.  Instead, 'next'
-- creates a sequential signal (i.e. a register).  Anything else is
-- combinatorial.

class Monad m => Seq m r where

  -- Register operation
  signal   :: SType -> m (r S)    -- Undriven signal
  next     :: r S -> r S -> m ()  -- Drive a register input

  -- Note that 'signal' and 'next' are considered to be imperative
  -- low-level primitives.  They should not be used in library code.
  -- Instead, use the declarative 'reg' operator.
  
  constant :: SType -> m (r S)
  stype    :: r S -> m SType

  -- Combinatorial operations all create driven intermediate signals
  -- to make them fit better in a monadic language.
  op2 :: Op2 -> r S -> r S -> m (r S)
  op1 :: Op1 -> r S -> m (r S)




-- Primitives

data Op2 = ADD | AND | XOR | SLL | SLR deriving Show
data Op1 = INV deriving Show

add :: forall m r. Seq m r => r S -> r S -> m (r S)
add = op2 ADD

band :: forall m r. Seq m r => r S -> r S -> m (r S)
band = op2 AND

bxor :: forall m r. Seq m r => r S -> r S -> m (r S)
bxor = op2 XOR

sll :: forall m r. Seq m r => r S -> r S -> m (r S)
sll = op2 SLL

slr :: forall m r. Seq m r => r S -> r S -> m (r S)
slr = op2 SLR

inv :: forall m r. Seq m r => r S -> m (r S)
inv = op1 INV


-- Declarative register feedback operator: 'signal' bundled with
-- 'next' avoids the creation of non-driven signals, or multiple
-- bindings through next.
reg :: Seq m r => SType -> (r S -> m (r S)) -> m (r S)
reg t f = do
  r <- signal t -- create undriven signal
  reg' r f

-- The non-declarative variant expecting an unbound signal.  This does
-- not have guarantees of reg, but might be convenient as a building
-- block.
reg' :: Seq m r => (r S) -> (r S -> m (r S)) -> m (r S)
reg' r f = do
  r' <- f r     -- create update equation with possible feedback
  next r r'     -- patch the register's input
  return r
