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
import Data.Bits

-- Compile to emulator.
--
-- This needs a fairly low-level emulation to be able to perform jumps
-- based on register values.

-- Due to label patching, compilation is a 2-pass algorithm to produce
-- an abstract code representation as an addressable map of machine
-- state transformations.  Code is an intermediate representation.
-- We're mostly interested in what we can do with it (see stateTrace).
type Code = Map Addr MachineTrans

compile :: EmuProg -> Code
compile = pass2 . pass1

-- No tagging needed for implementation.
type EmuProg = Emu ()
type Emu = WriterT [Patchable] (State (LabelNb, Addr, Labels)) -- see Pru instance
type Patchable = Either MachineTrans (LabelNb, Addr -> MachineTrans)
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr

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


-- Compilation state access
labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr
appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

-- Generic primitive compiler
comp :: [Patchable] -> EmuProg
comp ins = do
  modify $ appAddr (+ (4 * length ins))
  tell ins

-- Compile a patchable instruction
toPatch open = comp [ Right open ]

-- Compile a 1-tick instruction
noPatch closed = comp [ Left closed ]

  

-- Machine instructions are represented as state->state transformers.
-- The Machine is a map to integer values
type MachineState = Map MachineVar Int
-- Comprised of
data MachineVar
  = Reg Register  -- register file
  | CFlag         -- carry flag
  | PCounter      -- program counter
  deriving (Eq,Ord,Show)

-- State transformers are wrapped in a State monad for ease of
-- use and to allow later extensions.
type Machine = State MachineState
type MachineTrans = Machine ()

-- State variable access
store :: MachineVar -> Int -> MachineTrans
store var val = modify $ insert var val
load :: MachineVar -> Machine Int
load var = get >>= return . (! var)


instance Pru Emu where

  declare = do
    n <- labelNb
    modify $ appLabelNb (+ 1)
    return n
    
  label l = do
    a <- addr
    modify $ appLabels $ \ls -> insert l a ls

  -- Label use needs patching
  jmp l = toPatch $ (l, \a -> store PCounter a)

  -- Generic instructions
  ins2 Mov ra b = noPatch $ op b >>= store (Reg ra) >> next
  ins3 Add = int2 (+)

  -- Implemented as spin
  halt = noPatch $ return ()

-- Used in comp1
next :: MachineTrans
next =  modify $ adjust (+ 4) PCounter

-- Generic 2-operand Integer operations operations, truncated to 32
-- bit results.
int2 f ra rb c = noPatch $ do
  b' <- op $ R rb
  c' <- op c
  store (Reg ra) $ trunc32 $ b' + c'
  next

-- Generic operand dereference.
op (L lit) = return lit
op (R reg) = load $ Reg reg

trunc32 = (.&. 4294967295)  -- 0xFFFFFFFF



-- Running the abstract Code.

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

