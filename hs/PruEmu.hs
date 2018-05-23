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
  = File Int  -- register file
  | CFlag     -- carry flag
  | PCounter  -- program counter
  deriving (Eq,Ord,Show)

-- State transformers are wrapped in a State monad for ease of
-- use and to allow later extensions.
type Machine = State MachineState
type MachineTrans = Machine ()

-- State variable access
storem :: MachineVar -> Int -> MachineTrans
storem var val = modify $ insert var val

loadm :: MachineVar -> Machine Int
loadm var = get >>= return . (! var)

-- Registers are special: they have word, byte subaccess.
load :: R -> Machine Int
load (R r)    = loadm (File r)
load (Rw r w) = word 16 w <$> (loadm $ File r)
load (Rb r b) = word  8 b <$> (loadm $ File r)

mask bits = shift 1 bits
word bits w v = v' .&. (mask bits) where
  v' = shift v $ 0 - bits

trunc bits = (.&. (mask bits))

  
store :: R -> Int -> MachineTrans
store (R r) = (storem (File r)) . (trunc 32)
store (Rw r w) = store' 16 w (R r)
store (Rb r b) = store'  8 b (R r)

store' bits index (R r) sub = do
  old <- loadm (File r)
  let shift' = shift $ bits * index
      kill   = complement $ shift' (mask bits)
      sub'   = shift' sub
      old'   = old .&. kill
  storem (File r) $ old' .|. sub'
  

clrbit val bit = val .&. (complement $ shift 1 bit)
setbit val bit = val .|. (shift 1 bit)

instance Pru Emu where

  declare = do
    n <- labelNb
    modify $ appLabelNb (+ 1)
    return n
    
  label l = do
    a <- addr
    modify $ appLabels $ \ls -> insert l a ls

  -- Label use needs patching
  jmp l = toPatch $ (l, \a -> storem PCounter a)

  -- Generic instructions
  ins2r MOV ra rb = movop ra (Reg rb)
  ins2i LDI ra ib = movop ra (Im ib)
  ins3 ADD = intop2 (+)
  ins3 CLR = intop2 clrbit
  ins3 SET = intop2 setbit

  -- Implemented as spin
  halt = noPatch $ return ()

-- Used in comp1
next :: MachineTrans
next =  modify $ adjust (+ 4) PCounter

-- Generic 2-operand Integer operations operations, truncated to 32
-- bit results.
intop2 :: (Int -> Int -> Int) -> R -> R -> O -> Emu ()
intop2 f ra rb c = noPatch $ do
  b' <- op $ Reg rb
  c' <- op c
  store ra $ b' + c'
  next

-- Generic move.  On PRU this is split into two instructions: MOV and
-- LDI, instead of one instruction that can take multiple operand
-- types.
movop ra b = noPatch $ do
  v <- op b
  store ra v
  next


-- Generic operand dereference.
op (Im  im)  = return im
op (Reg r)   = load r






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

