{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module PruEmu(compile,stateTrace,Emu,EmuProg,MachineState(..),MachineVar(..)) where

import Pru
import Data.Map.Strict (Map, (!), empty, insert, fromList, adjust)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer

-- Compile to emulator.
--
-- This needs a fairly low-level emulation to be able to perform jumps
-- based on register values.

-- Due to label patching, compilation is a 2-pass algorithm.
compile :: EmuProg -> Map Addr MachineTrans
compile = pass2 . pass1

-- First pass uses a stack of a writer monad and a state monad.
pass1 :: EmuProg -> (Labels, [Patchable])
pass1 m = (labels, patchable) where
  (((), w), s) = runState (runWriterT m) (0, 0, empty)
  patchable = w
  (_, _, labels) = s

-- Second pass patches all unresolved addresses.
pass2 (labels, patchable) = code where
  code = fromList $ zip [0,4..] ins 
  ins = map patch patchable
  patch (Left ins) = ins
  patch (Right (label, addr2ins)) = addr2ins $ labels ! label
  

-- Only the instance needs a newtype.
type Emu = WriterT [Patchable] (State (LabelNb, Addr, Labels))
type EmuProg = Emu ()
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr

-- Patchable instruction
type Patchable = Either MachineTrans (LabelNb, Addr -> MachineTrans)

labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr

appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

asm :: [Patchable] -> EmuProg
asm lst = do
  modify $ appAddr (+ 4)
  tell lst

-- Machine instructions are state->state transformers.  Wrapped in a
-- State monad for ease of extension later.
type Machine = State MachineState
type MachineTrans = Machine ()

setVar :: MachineVar -> Int -> MachineTrans
setVar var val =
  modify $ insert var val

getVar :: MachineVar -> Machine Int
getVar var = get >>= return . (! var)

tick :: MachineTrans
tick =  modify $ adjust (+ 4) PCounter

-- Third pass is the evaluation, which starts from a run-time state.
-- Seems easiest to just use a Map with some well-defined keys.

data MachineVar = StateR R | PCounter deriving (Eq,Ord,Show)
type MachineState = Map MachineVar Int

instance Pru Emu where

  label = do
    n <- labelNb
    modify $ appLabelNb (+ 1)
    return n
    
  block l = do
    a <- addr
    modify $ appLabels $ \ls -> insert l a ls
    
  jmp l = do
    asm $ [Right $ (l, \addr -> setVar PCounter addr)]
    
  op2 Mov (OpR a) (OpR b) = asm [Left trans] where
    trans = do
      v <- getVar $ StateR b
      setVar (StateR a) v
      tick


-- One part particular interpretation of programs we're interested in
-- is state traces.  These can then be filtered to produce a specific
-- signal.  A program produces a sequence of machines states.

-- Note: it is assumed all instructions take exactly one cycle.

stateTrace :: MachineState -> EmuProg -> [MachineState]
stateTrace s0 prog = (s0 : next s0) where
  code = compile prog
  next s = (s' : next s') where
    pc = s ! PCounter
    ins = code ! pc
    s' = execState ins s

