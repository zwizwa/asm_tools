
-- Forth compilation stack exposed as a monad.

-- A little old-school, and not sure if there is a simpler way to do
-- this, but it seems to work really well as an alternative to a full
-- assembler.

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

compile = snd . compile'

compile' :: M t -> (t, [Instruction])
compile' m = (v, code') where
  ((v, code), (cstack, _)) =
    runState (runWriterT (unM m)) ([],0)
  code' = case cstack of
    [] -> code
    _  -> error $
      "Forth.compile: cstack not empty: " ++ show cstack

get' = get :: M CompState

advance n  = do (stk,cur) <- get' ; put (stk, cur + n)
cpush addr = do (stk,cur) <- get' ; put (addr:stk, cur)
here       = do (stk,cur) <- get' ; return cur
cpop       = do (addr:stk,cur) <- get' ; put (stk,cur) ; return addr
mark       = here >>= cpush
  
save inss  = do advance $ length inss ; tell inss





-- There is no forward declaration, so mutual recursion is not
-- possible.  Maybe that is a feature?  This construct allows nested
-- procedures.

-- FIXME: it might be possible to use knot-tying.


-- To compile an entire program, a meta construct is necessary to
-- insert a jump instruction at the beginning.
program :: (Address -> Instruction) -> M Address -> [Instruction]
program asm_jmp decls = (jump:code) where
  (start, (_:code)) = compile' $ do save [0] ; decls
  jump = asm_jmp start

-- Creating procedures can be expressed inside the monad.  The inner
-- routine compiles and returns the address.
fun' code = do
  label <- here
  code
  return label

-- A wrapper that creates a macro to perform a call.
fun :: (Address -> Instruction) -> M () -> M (M ())
fun asm_call code = do
  addr <- fun' code
  return $ save [asm_call addr]

-- See test-qc-SeqLib.hs for an example.


-- FIXME: the obvious next step is to eliminate tail calls.  This
-- could be done by abstracting ret as a compilation stack operation.
-- But, in absence of recursion, there is no need for eliminating tail
-- calls other than reducing stack usage.  Stack depth is cheap though.



  
  

