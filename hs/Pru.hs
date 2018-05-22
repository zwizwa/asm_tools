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

data Op = OpR R           deriving (Show, Eq, Ord)
data Opc2 = Mov           deriving Show
data Ins = Op2 Opc2 Op Op
         | Jmp Label      
         | Label Label    deriving Show
type Label = Int

type R = Int


r10 = OpR 10
r11 = OpR 11

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
  

-- Compile to emulator.
--
-- This needs a fairly low-level emulation to be able to perform jumps
-- based on register values.

-- Due to label patching, compilation is a 2-pass algorithm.
compile :: Emu () -> Map Addr MachineTrans
compile = pass2 . pass1

-- First pass uses a stack of a writer monad and a state monad.
pass1 :: Emu () -> (Labels, [Patchable])
pass1 m = (labels, pins) where
  (((), w), s) = runState (runWriterT m) (0, 0, empty)
  pins = w
  (_, _, labels) = s

-- Second pass patches all unresolved addresses.
pass2 (labels, pins) = code where
  ins = map resolve pins
  resolve (Left ins) = ins
  resolve (Right (label, ins')) = ins' $ labels ! label
  code = fromList $ zip [0,4..] ins 


type Emu = WriterT [Patchable] (State (LabelNb, Addr, Labels))
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr

-- Patchable instruction
type Patchable = Either MachineTrans (LabelNb, Addr -> MachineTrans)
type MachineTrans = MachineState -> MachineState

labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr

modifyLabelNb f = modify $ \(n,a,l) -> (f n, a, l)
modifyAddr    f = modify $ \(n,a,l) -> (n, f a, l)
modifyLabels  f = modify $ \(n,a,l) -> (n, a, f l)

asm :: [Patchable] -> Emu ()
asm lst = do
  modifyAddr (+ 4)
  tell lst

-- Third pass is the evaluation, which starts from a run-time state.
-- Seems easiest to just use a Map with some well-defined keys.

data MachineVar = StateR R | PC deriving (Eq,Ord,Show)
type MachineState = Map MachineVar Int

instance Pru Emu where

  label = do
    n <- labelNb
    modifyLabelNb (+ 1)
    return n
    
  block l = do
    a <- addr
    modifyLabels $ \ls -> insert l a ls
    
  jmp l = do
    asm $ [Right $ (l, \addr -> insert PC addr)]
    
  op2 Mov (OpR a) (OpR b) = asm [Left trans] where
    trans s = s' where
      v = s ! StateR b
      s' = insert (StateR a) v s


-- Get PC, find instruction, evaluate state transition, repeat.
trace :: Emu () -> MachineState -> [MachineState]
trace prog = next where
  code = compile prog
  next s = (s' : next s') where
    pc = s ! PC
    ins = code ! pc
    s' = ins s

