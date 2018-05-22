{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Pru where
import Data.Map.Strict (Map, (!), empty, insert, fromList)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer





class Monad m => Pru m where
  label :: m Label
  block :: Label -> m ()
  jmp :: Label -> m ()
  op2 :: Opc2 -> Op -> Op -> m ()

data Op = Reg String      deriving Show
data Opc2 = Mov           deriving Show
data Ins = Op2 Opc2 Op Op
         | Jmp Label      
         | Label Label    deriving Show
type Label = Int

r10 = Reg "R10"
r11 = Reg "R11"

mov :: Pru m => Op -> Op -> m ()
mov = op2 Mov

-- As a convenience.  The explicit dereference is quite annoying when
-- the reference producer doesn't have any side effects.
o2 f a b = do
  a' <- a
  b' <- b
  f a' b'

-- Debug: print to console.
instance Pru IO where
  label     = return $ 0
  block l   = print l
  op2 o a b = print $ Op2 o a b
  jmp l     = print $ Jmp l
  

-- Compiler for emulator.  Some design constraints and simplifications:
--
-- This needs a fairly low-level emulation to be able to perform jumps
-- based on register values.

-- The only post-procesing required is label patching, so this is a
-- 2-pass algorithm.  To allow sparse representation, store values in
-- an Int-indexed map.

-- This is a combination of a writer monad and a state monad.  Time to
-- start using monad stacks.

type Pass1 = WriterT [PIns] (State (LabelNb, Addr, Labels))
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr


labelNb = get >>= \(n,_,_) -> return n :: Pass1 LabelNb
addr    = get >>= \(_,a,_) -> return a :: Pass1 Addr

modifyLabelNb f = modify $ \(n,a,l) -> (f n, a, l)
modifyAddr    f = modify $ \(n,a,l) -> (n, f a, l)
modifyLabels  f = modify $ \(n,a,l) -> (n, a, f l)


-- Patchable instruction
type PIns = Either Ins (LabelNb, Addr -> Ins)

compile1 :: Pass1 () -> (Labels, [PIns])
compile1 m = (labels, pins) where
  (((), w), s) = runState (runWriterT m) (0, 0, empty)
  pins = w
  (_, _, labels) = s

compile m = ins where
  (labels, pins) = compile1 m
  ins = map resolve pins
  resolve (Left ins) = ins
  resolve (Right (label, ins')) = ins' $ labels ! label


asm :: [PIns] -> Pass1 ()
asm lst = do
  modifyAddr (+ 4)
  tell lst

instance Pru Pass1 where

  label = do
    n <- labelNb
    modifyLabelNb (+ 1)
    return n
    
  block l = do
    a <- addr
    modifyLabels $ \ls -> insert l a ls
    
  jmp l = do
    asm $ [Right $ (l, \addr -> Jmp addr)]
    
  op2 o a b = do
    asm $ [Left $ Op2 o a b]
    

  
  
