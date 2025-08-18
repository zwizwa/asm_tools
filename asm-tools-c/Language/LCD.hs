-- STUB

-- Tagless final language to generate C code or run in the ST Monad.
-- Basically I want a subset of C (Lowest Common Denominator) that can
-- be ported to other languages as well.

-- I think I want to use this for writing state machines as well,
-- which means that it won't be readable C because it will just have
-- if and goto as control structures.

-- The point is that it is C, not readable C.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Language.LCD where

import Control.Monad.Fix

data LCDPrim = Add | Sub | Mul | Abs | Signum deriving Show

data Label = Label Int deriving Show

class MonadFix m => LCD m r | m -> r, r -> m where
  op1 :: Num t => LCDPrim -> r t        -> m (r t)
  op2 :: Num t => LCDPrim -> r t -> r t -> m (r t)

  -- Note that lambda is not necessary if there are no higher order
  -- functions.  Only need things like 'for'

  -- Tuples are needed if functions are unary.

  -- What do I want to do with this?  Mostly writing DSP test code but
  -- that can be done with SynthTools as well.
                        
function test1 = mdo
  return ()
