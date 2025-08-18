-- STUB

-- Tagless final language to generate stackless C state machine code.
-- See LCD for code that runs on the C stack instead.

-- Idea in progress

-- Similar to Language.Neon code emulator: compile to graph first,
-- then execute.  All while maintaining fairly direct relation to C
-- syntax to make it really easy to generate and stick close to C
-- semantics.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Language.SMC where

import Control.Monad.Fix

data SMCPrim = Add | Sub | Mul | Abs | Signum deriving Show

data Label = Label Int deriving Show

class MonadFix m => SMC m r | m -> r, r -> m where
  op1 :: Num t => SMCPrim -> r t        -> m (r t)
  op2 :: Num t => SMCPrim -> r t -> r t -> m (r t)

  label :: m (r Label)    -- Current code address

  
function test1 = mdo
  
  f1 <- label

  f2 <- label

  return ()
