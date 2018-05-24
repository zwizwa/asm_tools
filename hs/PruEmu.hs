{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module PruEmu(compile,stateTrace,Emu,EmuSource,MachineState(..),MachineVar(..)) where

import Pru
import Data.Map.Strict (Map, (!), lookup, empty, insert, fromList, adjust)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Bits

-- Compile to emulator.
--
-- Emulation needs a fairly low-level emulation to be able to perform
-- jumps based on register values.  The PRU can do this in a single
-- cycle, which is what makes it so useful for signal generation.

-- Assembly generally requires a 2-pass algorithm for address
-- resolution, but here we can use circular programming.  Also see:
-- https://wiki.haskell.org/wikiupload/1/14/TMR-Issue6.pdf

-- EmuCompiled is the compiled form of EmuSource, ready to be emulate
-- run-time behavior.  We're mostly interested in the latter (see
-- stateTrace).

type EmuSource = Emu ()                       -- embedded source
type EmuCompiled = Map Addr MachineOp         -- compiled emulator functions
type Emu = ReaderT Link                       -- main monad
          (WriterT [MachineOp]
          (State (LabelNb, Addr, Labels)))
type Link = (LabelNb -> Addr)                 -- address resolution
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr


compile :: EmuSource -> EmuCompiled
compile m = code  where
  s0 = (0, 0, empty)
  (((), w), s) = runState (runWriterT (runReaderT m r)) s0
  code = fromList $ zip [0,1..] w
  (_, _, labels) = s
  r = (labels !) -- circular


-- Compilation state access
labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr
appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

-- Compile a patchable instruction
comp :: MachineOp -> EmuSource
comp ins = do
  tell [ins]
  modify $ appAddr (+ 1)


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
type MachineOp = Machine ()

-- State variable access
storem :: MachineVar -> Int -> MachineOp
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

  
store :: R -> Int -> MachineOp
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

-- Generic run time operand dereference.
op (Im (I im))  = return im
op (Im (L l))   = error $ "label " ++ show l ++ " not resolved"
op (Reg r)      = load r

-- Compile time resolution.
-- Label resolution is not in the evaluation path that computes the
-- table, so circular progamming works here.
link (Im (L l)) = do a <- ask ; return $ Im (I (a l))
link o          = return $ o

instance Pru Emu where

  declare = do
    n <- labelNb
    modify $ appLabelNb (+ 1)
    return $ L n
    
  label (L l) = do
    a <- addr
    modify $ appLabels $ \ls -> insert l a ls

  inso JMP = compo $ storem PCounter
    
  insro JAL (R r) = compo $ \o' -> do
      pc <- loadm PCounter
      storem (File r) (pc + 1)
      storem PCounter o'
    
  -- FIXME: all immediates can all be addresses.  It might be simpler
  -- to just link all the arguments.
    
  -- Generic instructions
  insrr MOV ra rb = movop ra (Reg rb)
  insri LDI ra ib = movop ra (Im ib)
  insrro ADD = intop2 (+)
  insrro CLR = intop2 clrbit
  insrro SET = intop2 setbit
  insiri XOUT _ _ _ = comp next -- FIXME

  -- Implemented as spin
  ins HALT = comp $ return ()
  ins NOP  = comp next

  comment _ = return ()

-- Adjust state to resume at the next instruction
next :: MachineOp
next = modify $ adjust (+ 1) PCounter

-- Convenient 2-level dereference of operand to Int.
-- Note the difference between:
-- link: compile time lookup (label -> addr)
-- op:   run time lookup (reg -> value)
compo :: (Int -> MachineOp) -> O -> EmuSource
compo f o = do
  o' <- link o
  comp $ do
    o'' <- op o'
    f o''

-- Generic move.  On PRU this is split into two instructions: MOV and
-- LDI, instead of one instruction that can take multiple operand
-- types.
movop ra = compo $ \b -> do
  store ra b
  next

-- Generic 2-operand Integer operations.
intop2 :: (Int -> Int -> Int) -> R -> R -> O -> Emu ()
intop2 f ra rb c = do
  c' <- link c
  comp $ do
    b'  <- op $ Reg rb
    c'' <- op c'
    store ra $ f b' c''
    next


-- Running the EmuCompiled code.


-- Normal machine cycle.
tick :: EmuCompiled -> MachineOp
tick code = do
  pc <- gets (! PCounter)     -- Read program counter from state
  code ! pc                   -- Run instruction, which updates PCounter
  modify $ adjust (+ 1) Time  -- FIXME: Assumes 1 cycle / instruction


-- One particular interpretation of programs we're interested in is
-- state traces.  These can then be filtered to isolate a specific
-- signal.  A program produces a sequence of machines states.

stateTrace ::
  MachineState ->                    -- Initial state
  (MachineState -> MachineState) ->  -- External IO effects
  EmuCompiled ->
  [MachineState]
stateTrace s0 io code = s0 : evalState stateSeq s0 where
  stateSeq = sequence $ cycle [tick']
  tick' = do
    modify io   -- Apply external influence
    tick code   -- Normal machine cycle
    get         -- return state for trace


