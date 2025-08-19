-- STUB

-- Tagless final language to generate an Erlang-style pure language
-- with send and receive primitives.

-- The basic idea here is that it is possible to have an expression
-- sublanguage that can be recoverd using data-reify but still have
-- some imperative constructs (send / recv).

-- This resolves a previous confusion that it is necessary to have a
-- monad for sharing.  It is much better to have data-reify because
-- that allows porting of all the Haskell expression stuff without the
-- monad getting in the way.

-- I actually have a potential target for this: the Lua Actor language.



{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Language.MiniErl where

import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Writer


data Prim = Add | Sub | Mul | Abs | Signum deriving Show
data Proc = Proc Int

class MonadFix m => Erl m r | m -> r, r -> m where
  op1 :: Num t => Prim -> r t        -> r t
  op2 :: Num t => Prim -> r t -> r t -> r t
  recv :: m (r t)
  send :: r Proc -> r t -> m ()
  
  


  

data Comp t

data Code = Code Int deriving (Show)
data CodeState = CodeState deriving (Show)

newtype CompM t = CompM (WriterT [Code] (State CodeState) t)
                deriving (Functor, Applicative, Monad,
                          MonadFix,
                          MonadWriter [Code],
                          MonadState CodeState)

instance Erl CompM Comp where
  op1 = undefined
  op2 = undefined
  recv = undefined
  send = undefined
  
 
