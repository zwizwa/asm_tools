{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module PruEmu(compile,stateTrace,Emu,EmuProg,MachineState(..),MachineVar(..)) where

import Pru
import Data.Map.Strict (Map, (!), lookup, empty, insert, fromList, adjust)
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
type EmuCode = Map Addr MachineTrans

compile :: EmuProg -> EmuCode
compile = pass2 . pass1

-- No tagging needed for implementation.
type EmuProg = Emu ()
type Emu = WriterT [Patchable] (State (LabelNb, Addr, Labels)) -- see Pru instance
type Patchable = (LabelNb -> Addr) -> MachineTrans
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
  patch ins = ins (labels !)


-- Compilation state access
labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr
appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

-- Compile a patchable instruction
comp :: [Patchable] -> EmuProg
comp inss = do
  modify $ appAddr (+ (4 * length inss))
  tell inss

-- Compile a single instruction
comp' :: MachineTrans -> EmuProg
comp' ins  = comp [\_ -> ins]
  

-- Machine instructions are represented as state->state transformers.
-- The Machine is a map to integer values
type MachineState = Map MachineVar Int
-- Comprised of
data MachineVar
  = File Int  -- register file
  | CFlag     -- carry flag
  | PCounter  -- program counter
  | Time      -- instruction counter
  deriving (Eq,Ord,Show)

-- State transformers are wrapped in a State monad for ease of
-- use and to allow later extensions.
type Machine = State MachineState
type MachineTrans = Machine ()

-- State variable access
storem :: MachineVar -> Int -> MachineTrans
storem var val = modify $ insert var val

loadm :: MachineVar -> Machine Int
loadm var = do
  maybe <- gets $ (Map.lookup var)
  return $ checkVar var maybe

checkVar var (Just val) = val
checkVar var Nothing = error $ "Uninitialized MachineVar: " ++ show var


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
  jmp l = comp [\a -> storem PCounter (a l)]
  insro JAL (R r) (Im l) = comp  [ \a -> jalop r (a l) ]
  insro JAL (R r) o      = comp' $ op o >>= jalop r

        
  -- FIXME: immediates can all be addresses, so maybe make all
  -- instructions patchable?

  -- Generic instructions
  insrr MOV ra rb = movop ra (Reg rb)
  insri LDI ra ib = movop ra (Im ib)
  insrro ADD = intop2 (+)
  insrro CLR = intop2 clrbit
  insrro SET = intop2 setbit
  insiri XOUT _ _ _ = comp' next -- FIXME

  -- Implemented as spin
  ins HALT = comp' $ return ()
  ins NOP  = comp' next

  comment _ = return ()

-- Used in comp1
next :: MachineTrans
next =  modify $ adjust (+ 4) PCounter

-- Generic 2-operand Integer operations operations, truncated to 32
-- bit results.
intop2 :: (Int -> Int -> Int) -> R -> R -> O -> Emu ()
intop2 f ra rb c = comp' $ do
  b' <- op $ Reg rb
  c' <- op c
  store ra $ b' + c'
  next

-- Generic move.  On PRU this is split into two instructions: MOV and
-- LDI, instead of one instruction that can take multiple operand
-- types.
movop ra b = comp' $ do
  v <- op b
  store ra v
  next

jalop r nextpc = do
    pc <- loadm PCounter
    storem (File r) (pc + 4)
    storem PCounter nextpc


-- Generic operand dereference.
op (Im  im)  = return im
op (Reg r)   = load r






-- Running the abstract Code.

-- One part particular interpretation of programs we're interested in
-- is state traces.  These can then be filtered to produce a specific
-- signal.  A program produces a sequence of machines states.

-- Note: it is assumed all instructions take exactly one cycle.

stateTrace ::
  MachineState ->                    -- Initial state
  (MachineState -> MachineState) ->  -- External IO effects
  EmuCode ->
  [MachineState]
stateTrace s0 io code = s0 : evalState stateSeq s0 where
  stateSeq = sequence $ cycle [tick]
  tick = do
    modify io                   -- Apply external influence
    pc <- gets (! PCounter)     -- Program counter from state
    code ! pc                   -- Run code at PC
    modify $ adjust (+ 1) Time 
    get                         -- return state for trace

  
