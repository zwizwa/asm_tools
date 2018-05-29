-- An eDSL for sequential logic.

-- Note that this is not a full HDL.
-- Following wikipedia's definition, Seq implements an RTL language.
-- https://en.wikipedia.org/wiki/Register-transfer_level

-- It helps to think of HDLs as discrete event simulation languages,
-- with RTL embedded inside in the case synchronization points are
-- only determined by clocks.

-- Seq has implicit clock signals Only 'next' to relate the current
-- clock cycle to the next.


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module Seq where

import Control.Applicative
import Data.Traversable

-- Abstract tag for signal representation.
data S = S
data SType = SInt (Maybe NbBits) InitVal
type NbBits = Int
type InitVal = Int

-- Signals
--
-- A note on Signals.  Typical HDL semantics of signals:
-- 1) have exactly one driver
-- 2) is a wire if driven in combinatorial context
-- 3) is a register if driven in sequential context
--
-- We distinguish between combinatorial and register signals in the
-- following way:
--
-- a) The default is combinatorial.  This is abstracted as ordinary
--    functions taking value inputs and returning values.
--
-- b) Registers are created using 'signal' and bound using 'next'.
--    For the library, a higher level 'regFix' is exposed which avoids
--    creating unbound or multiply-bound signals.


-- Signal types.
--
-- It would be possible to encode target type information at the
-- Haskell type level.  However, I find that really awkward to do
-- without dependent types, so it is encoded as an SType value.  This
-- creates some awkwardness, i.e. probing with dummy values is
-- necessary at some points.
--
-- In practice, the Haskell run time is a compile time for the target
-- language, so target type errors can still be caught as Haskell run
-- time errors.  In the end, encoding SType as a Haskell type doesn't
-- really add much value for a target language that just has sized bit
-- vectors.


-- Signal containers.
--
-- The final output is a flat configuration of wires, logic gates and
-- registers. There isn't much to contain.  It makes sense not to put
-- containers at the target level as part of the Seq class.  Instead
-- they are introduced at the meta level, allowing standard Haskell
-- types to be used.  E.g. 'regs' assumes a Traversable Applicative.



class Monad m => Seq m r | r -> m where

  -- Register operation
  signal   :: SType -> m (r S)    -- Undriven signal
  next     :: r S -> r S -> m ()  -- Drive a register input
  connect  :: r S -> r S -> m ()  -- Combinatorial connect

  -- Note that 'signal' and 'next' are considered to be imperative
  -- low-level primitives.  They should not be used in library code.
  -- Instead, use the declarative 'reg' operator.
  -- Combinatorial connect is necessary for HDL export.
  
  constant :: SType -> r S
  stype    :: r S -> m SType

  -- Combinatorial operations all create driven intermediate signals
  -- to make them fit better in a monadic language.
  op1 :: Op1 -> r S -> m (r S)
  op2 :: Op2 -> r S -> r S -> m (r S)
  op3 :: Op3 -> r S -> r S -> r S -> m (r S)


-- Primitives

data Op1 = INV
  deriving Show

inv :: forall m r. Seq m r => r S -> m (r S)
inv = op1 INV

data Op2 = ADD | AND | XOR | SLL | SLR
  deriving Show

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

data Op3 = IF
  deriving Show

if' :: forall m r. Seq m r => r S -> r S -> r S -> m (r S)
if' = op3 IF  


-- Convert register update equation to to monadic output value,
-- "tucking away" the registers.  This effectively computes the fixed
-- point with register decoupling, defining a sequence.

-- Noe that using 'singal' and 'next' in the same function will avoid
-- the creation of undriven or multiply-driven signals.  This is
-- preferred over using the low-level primitives directly.

-- A meta-level applicative functor is used to bundle registers.
-- Typically, a List will do.
regFix ::
  (Applicative f, Traversable f, Seq m r) =>
  f SType -> (f (r S) -> m (f (r S), o)) -> m o
regFix ts f = do
  rs <- sequence $ fmap signal ts
  (rs', o) <- f rs
  sequence_ $ liftA2 next rs rs'
  return o

-- Note: it might be possible to avoid 'signal' and 'next' in Seq, and
-- replace it with regFix.  Currently, the MyHDL uses it to bind
-- outputs, but that can probably be solved differently.









