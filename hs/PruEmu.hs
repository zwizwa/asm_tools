{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module PruEmu(compile,stateTrace,Emu,EmuProg,MachineState(..),MachineVar(..)) where

import Pru
import Data.Map.Lazy (Map, (!), lookup, empty, insert, fromList, adjust)
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

-- Code is treated as an internal intermediate representation.  We're
-- mostly interested in what we can do with it (see stateTrace).

type EmuCode = Map Addr MachineTrans
type EmuProg = Emu ()
type Emu = ReaderT Link (WriterT [MachineTrans] (State (LabelNb, Addr, Labels))) -- see Pru instance
type Link = (LabelNb -> Addr)
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr


compile :: EmuProg -> EmuCode
compile m = code  where
  (((), w), s) = runState (runWriterT (runReaderT m e)) (0, 0, empty)
  code = fromList $ zip [0,1..] w
  (_, _, labels) = s
  e = (labels !) -- circular


-- Compilation state access
labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr
appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

-- Compile a patchable instruction
comp :: [MachineTrans] -> EmuProg
comp inss = do
  tell inss
  modify $ appAddr (+ (length inss))
comp' ins = comp [ins]  

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

-- Label resolution is not in the evaluation path that computes the
-- table, so circular progamming works here.
link l = do a <- ask ; return $ a l

instance Pru Emu where

  declare = do
    n <- labelNb
    modify $ appLabelNb (+ 1)
    return n
    
  label l = do
    a <- addr
    modify $ appLabels $ \ls -> insert l a ls

  jmp l = do
    a <- link l;
    comp' $ storem PCounter a
    
  insro JAL (R r) (Im l) = do
    a <- link l
    comp' $ jalop r a
    
  insro JAL (R r) o      = comp' $ op o >>= jalop r

        
  -- FIXME: immediates can all be addresses.  Not jet supported.
    
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
next =  modify $ adjust (+ 1) PCounter

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
    storem (File r) (pc + 1)
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

  
