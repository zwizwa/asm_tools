
-- Not a full Forth, just monad syntax for loop constructs.
-- This is mostly to avoid named labels.
-- See CPU.hs for an example.

-- EDIT: This is probably entirely unnecessary because loop bodies can
-- just go in do blocks, but let's keep it as an underlying structure
-- anyway.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Forth where

import Control.Monad.State
import Control.Monad.Writer


type Program = [Instruction]
type Instruction = Int
type Address = Int
type ControlStack = [Address]
type CompState = (ControlStack, Address)

newtype M t = M { unM ::
                    WriterT Program
                    (State CompState)
                    t
                } deriving
  (Functor, Applicative, Monad,
   MonadWriter Program,
   MonadState CompState)

compile :: M () -> [Instruction]
compile m = 
  case runState (runWriterT (unM m)) ([],0) of
    (((), code), ([], _)) -> code
    v -> error $ "Forth.comple: " ++ show v

get' = get :: M CompState

advance n  = do (stk,cur) <- get' ; put (stk, cur + n)
cpush addr = do (stk,cur) <- get' ; put (addr:stk, cur)
here       = do (stk,cur) <- get' ; return cur
cpop       = do (addr:stk,cur) <- get' ; put (stk,cur) ; return addr
mark       = here >>= cpush
  
save inss  = do advance $ length inss ; tell inss

