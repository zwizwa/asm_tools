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
import Data.Foldable
import Data.Traversable
import Data.Key hiding((!))
import Prelude hiding(zipWith)

-- Abstract tag for signal representation.
data S = S
type SSize = Maybe NbBits
data SType = SInt SSize InitVal deriving Show
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
--    For the library, a higher level 'fixReg' is exposed which avoids
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


-- Constants
--
-- Note that constant is non-monadic.  This emposes some constraints
-- on the implementation in how to represent them, which is a fair
-- price for the convenience of using them in-line.  Num is there
-- mostly to support fromInteger, but the additional methods could be
-- useful.  Note that consts have the same type as other nodes, which
-- possibly leads to run-time error when applying Num methods to
-- register nodes.  The implementation should raise an error in that
-- case.

-- Integers
--
-- Ints currently behave as MyHDL's modbv with min=0 and max=2^NbBits.
-- This needs to be fixed at some point, but wait for application pull.

class (Monad m, Num (r S)) => Seq m r | r -> m where

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

  -- Slices are special: there's a big difference between constant
  -- stlices (just wires) and dynamic slices (barrel shifter), so
  -- limit to constant slices only.  Upper bound can be Nothing, which
  -- takes all the upper bits.
  slice :: r S -> SSize -> NbBits -> m (r S)


-- Primitives

data Op1 = INV
  deriving Show

inv :: forall m r. Seq m r => r S -> m (r S)
inv = op1 INV

data Op2 = ADD | MUL | AND | OR | XOR | SLL | SLR | CONC | EQU
  deriving Show

add :: forall m r. Seq m r => r S -> r S -> m (r S)
add = op2 ADD

mul :: forall m r. Seq m r => r S -> r S -> m (r S)
mul = op2 MUL

equ :: forall m r. Seq m r => r S -> r S -> m (r S)
equ = op2 EQU

band :: forall m r. Seq m r => r S -> r S -> m (r S)
band = op2 AND

bxor :: forall m r. Seq m r => r S -> r S -> m (r S)
bxor = op2 XOR

bor :: forall m r. Seq m r => r S -> r S -> m (r S)
bor = op2 OR

sll :: forall m r. Seq m r => r S -> r S -> m (r S)
sll = op2 SLL

slr :: forall m r. Seq m r => r S -> r S -> m (r S)
slr = op2 SLR

conc :: forall m r. Seq m r => r S -> r S -> m (r S)
conc = op2 CONC

data Op3 = IF
  deriving Show

if' :: forall m r. Seq m r => r S -> r S -> r S -> m (r S)
if' = op3 IF  


-- Convert register update equation to to monadic output value,
-- "tucking away" the registers.  This effectively computes the fixed
-- point with register decoupling, defining a sequence.

-- Noe that using 'signal' and 'next' in the same function will avoid
-- the creation of undriven or multiply-driven signals.  This is
-- preferred over using the low-level primitives directly.

-- A meta-level container is used to bundle registers.  Typically, a
-- List will do, but the interface is generic.  Note: liftA2 doesn't
-- do the right thing on lists.

closeReg :: (Zip f, Traversable f, Seq m r) =>
  f SType -> (f (r S) -> m (f (r S), o)) -> m o
closeReg ts f = do
  rs <- sequence $ fmap signal ts
  (rs', o) <- f rs
  sequence_ $ zipWith next rs rs'
  return o


-- Note: it might be possible to avoid 'signal' and 'next' in Seq, and
-- replace it with closeReg.  Currently, the MyHDL uses it to bind
-- outputs, but that can probably be solved differently.

-- Can Num be implemented generically?




