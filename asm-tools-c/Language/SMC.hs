-- WIP.

-- Design Ideas:
--
-- . Tagless final language to express state machines that can run on
--   C (computed goto or trampoline) or be implemented on a lower
--   level substrate (programmable datapath or small CPU in FPGA).
--
-- . Keep the expression language separate, for better Haskell
--   embedding.  This also makes mapping to substrate more flexible.
--
-- . Use data-reify to get the concrete syntax.
--
-- . Different from LCD which aims to build on C function calls
--   instead ( and might be abandoned. )
--

-- Implementation Ideas:
--
-- . Eval representation is similar to Language.Neon code emulator:
--   compile to graph first, then execute.  All while maintaining
--   fairly direct relation to C syntax to make it really easy to
--   generate and stick close to C semantics.


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- ? Fixed after using more concrete base types

module Language.SMC where

import Control.Monad.Fix
import Control.Monad.Writer
import qualified Control.Monad.State as State

data Prim = Add | Sub | Mul | Abs | Signum deriving Show

data Label = Label Int deriving Show

class Type t where

instance Type Int where
  
-- Phantom type for compiler, concrete type for emuator.
data Var t = Var t
  
class Expr r where
  op1 :: Type t => Prim -> r t        -> r t
  op2 :: Type t => Prim -> r t -> r t -> r t
  lit :: Type t => t -> r t

class (MonadFix m, Expr r) => Ctrl m r | m -> r, r -> m where
  block :: m () -> m (r Label)  -- Can be jumped to
  goto  :: r Label -> m ()      -- Jump to

class Expr r => Mem m r | m -> r, r -> m where
  var :: r t -> m (r (Var t))
  set :: r (Var t) -> r t -> m ()
  get :: r (Var t) -> m (r t)

test1 :: (Ctrl m r, Mem m r) => Int -> m ()
test1 init = mdo

  v <- var $ lit init
  
  f1 <- block $ do
    v' <- get v
    let x = op2 Add v' v'
        y = op2 Mul x x
    set v y
    goto f2
        
  f2 <- block $ do
    goto f1

  return ()



data C t

data CCode = CCode Int deriving (Show)
data CState = CState deriving (Show)

newtype CM t = CM (WriterT [CCode] (State.State CState) t)
             deriving (Functor, Applicative, Monad,
                       MonadFix,
                       MonadWriter [CCode],
                       State.MonadState CState)

instance Ctrl CM C where
  label = undefined
  goto  = undefined

instance Expr C where
  op1 = undefined
  op2 = undefined
  lit = undefined
