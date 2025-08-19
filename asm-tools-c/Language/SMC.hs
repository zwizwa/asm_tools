-- STUB

-- Tagless final language to generate stackless C state machine code.
-- See LCD for code that runs on the C stack instead.

-- Idea in progress

-- Similar to Language.Neon code emulator: compile to graph first,
-- then execute.  All while maintaining fairly direct relation to C
-- syntax to make it really easy to generate and stick close to C
-- semantics.

-- THE MAIN POINT
--
-- Keep the expression langauge part as pure functions in Haskell, and
-- only expose the impure parts as monadic ops.  The data-reify
-- approach should work just fine.


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
import Control.Monad.State
import Control.Monad.Writer


data SMCPrim = Add | Sub | Mul | Abs | Signum deriving Show

data Label = Label Int deriving Show

class MonadFix m => SMC m r | m -> r, r -> m where
  op1 :: Num t => SMCPrim -> r t        -> r t
  op2 :: Num t => SMCPrim -> r t -> r t -> r t
  lit :: Num t => t -> r t

  label :: m (r Label)    -- Current code address

  
function test1 = mdo
  
  f1 <- label
  x <- op1 Add (lit 1) (lit 2)

  f2 <- label

  return ()

data C t

data CCode = CCode Int deriving (Show)
data CState = CState deriving (Show)

newtype CM t = CM (WriterT [CCode] (State CState) t)
             deriving (Functor, Applicative, Monad,
                       MonadFix,
                       MonadWriter [CCode],
                       MonadState CState)



instance SMC CM C where
  op1 = undefined
  op2 = undefined
  label = undefined
  lit = undefined
