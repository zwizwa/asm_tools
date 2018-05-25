{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PruEmu(compile,compile',Emu,EmuSrc,EmuCode,
              MachineState(..),MachineVar(..),MachineOp,
              logTrace, logTrace', EmuLog(..),
              machineInit,machineInit0,machineInit',
              stateTrace) where

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

-- EmuCode is the compiled form of EmuSrc.  Each memory address
-- contains a function that emulates the instruction at that location.

type EmuSrc = Emu ()                       -- embedded source
type EmuCode = Map Addr MachineOp          -- compiled emulator functions
type Emu = ReaderT Link                    -- main monad
          (WriterT [Either PseudoOp MachineOp]
          (State (LabelNb, Addr, Labels)))
type Link = (LabelNb -> Addr)              -- address resolution
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr


compile = fst . compile'

compile' :: EmuSrc -> (EmuCode, Labels)
compile' m = (code, labels)  where
  s0 = (0, 0, empty)
  (((), w), s) = runState (runWriterT (runReaderT m r)) s0
  w' = mergePre w
  code = fromList $ zip [0,1..] w'
  (_, _, labels) = s
  r = (labels !) -- circular

-- zero-width instrumentation instructions are prefixed to the first
-- actual instruction. FIXME: This feels a bit hacky.  Maybe Reader is
-- not the right tool for this.
mergePre :: [Either MachineOp MachineOp] -> [MachineOp]
mergePre = f0 where
  f0 = f $ return ()
  f pre [] = [pre] -- insert pseudo instruction at the end
  f pre ((Right m):cells) = (pre >> m) : (f0 cells)
  f pre ((Left  m):cells) = f (pre >> m) cells


-- Compilation state access
labelNb = get >>= \(n,_,_) -> return n :: Emu LabelNb
addr    = get >>= \(_,a,_) -> return a :: Emu Addr
appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

-- Compile a (pseudo) instruction
comp :: MachineOp -> EmuSrc
comp ins = do tell [Right ins] ; modify $ appAddr (+ 1)

pseudo :: PseudoOp -> EmuSrc
pseudo ins = tell [Left ins]

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

-- State transformers are wrapped in a State monad.
-- The log is abstract to allow custom tracing types.
newtype EmuMachine w t = EmuMachine { unEmuMachine :: (WriterT w (State MachineState) t) }
  deriving (Functor, Applicative, Monad, MonadWriter w, MonadState MachineState)
type Machine = EmuMachine [EmuLog]
type MachineOp = Machine ()
type PseudoOp = MachineOp

-- State variable access
storem :: MachineVar -> Int -> MachineOp
storem var val = do
  modify $ insert var val


loadm :: MachineVar -> Machine Int
loadm var = do
  maybe <- gets $ (Map.lookup var)
  return $ checkVar var maybe

checkVar var (Just val) = val
checkVar var Nothing = error $ "Uninitialized MachineVar: " ++ show var

-- Ideally, the log would be a type parameter, but it seems quite a
-- bit of work to make that change.  For now just support some basic
-- hardcoded types: generic strings, and string-tagged machine state
-- snapshots.
data EmuLog = LogString String | LogState String MachineState deriving Show



-- Registers are special: they have word, byte subaccess.
load :: R -> Machine Int
load (R r)    = loadm (File r)
load (Rw r w) = word 16 w <$> (loadm $ File r)
load (Rb r b) = word  8 b <$> (loadm $ File r)

mask bits = (shift 1 bits) - 1
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
ref (Im (I im))  = return im
ref (Im (L l))   = error $ "label " ++ show l ++ " not resolved"
ref (Reg r)      = load r

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

  inso JMP = comp_link_ref $ storem PCounter
    
  insro JAL (R r) = comp_link_ref $ \o -> do
      pc <- loadm PCounter
      storem (File r) (pc + 1)
      storem PCounter o
    
  -- FIXME: all immediates can all be addresses.  It might be simpler
  -- to just link all the arguments.
    
  -- Generic instructions
  insrr MOV ra rb = move ra (Reg rb)
  insri LDI ra ib = move ra (Im ib)
  insrro ADD = intop2 (+)
  insrro CLR = intop2 clrbit
  insrro SET = intop2 setbit
  insiri XOUT _ _ _ = comp next -- FIXME

  -- Implemented as spin
  ins HALT = comp $ return ()
  ins NOP  = comp next

  -- Instrumentation
  logStr str   = pseudo $ tell [LogString str]
  snapshot tag = pseudo $ do s <- get ; tell [LogState tag s]


-- Adjust state to resume at the next instruction
next :: MachineOp
next = modify $ adjust (+ 1) PCounter

-- Convenient 2-level dereference of operand to Int.
-- Note the difference between:
-- link: compile time lookup (label -> addr)
-- ref:  run time lookup (reg -> value)
comp_link_ref :: (Int -> MachineOp) -> O -> EmuSrc
comp_link_ref f o = do
  o' <- link o
  comp $ do
    o'' <- ref o'
    f o''

-- Generic move.  On PRU this is split into two instructions: MOV and
-- LDI, instead of one instruction that can take multiple operand
-- types.
move ra = comp_link_ref $ \b -> do
  store ra b
  next

-- Generic 2-operand Integer operations.
intop2 :: (Int -> Int -> Int) -> R -> R -> O -> Emu ()
intop2 f ra rb c = do
  c' <- link c
  comp $ do
    b'  <- ref $ Reg rb
    c'' <- ref c'
    store ra $ f b' c''
    next


-- Running the EmuCode code.


-- Normal machine cycle.
tick :: EmuCode -> MachineOp
tick code = do
  pc <- gets (! PCounter)     -- Read program counter from state
  code ! pc                   -- Run instruction, which updates PCounter
  modify $ adjust (+ 1) Time  -- FIXME: Assumes 1 cycle / instruction


-- Run machine with custom per-tick tracer.
-- Machine runs indefinitely producing a lazy stream.
tickTrace ::
  EmuCode ->         -- Compiled assembly listing
  Machine t ->       -- Runs before tick
  MachineState ->    -- Initial state
  [t]
tickTrace code preTick s = seq where
  (seq, _) = evalState (runWriterT $ unEmuMachine mseq) s
  mseq = sequence $ cycle [tick']
  tick' = do
    s <- preTick  -- User-provided action
    tick code     -- Normal machine cycle
    return s

-- One particular interpretation of programs we're interested in is
-- state traces.  These can then be filtered to isolate a specific
-- signal.  
stateTrace code pre =
  tickTrace code $ do s <- get ; pre ; return s


-- Note that the Writer log is different.  The machine cannot be run
-- with an infinite program as above.  The final log result is only
-- available _after_ the computation has finished, so the program
-- needs to be finite.  However it is possible to "chunk" it like
-- below.  Is there a more elegant way?

-- Run for a finite amount of time.
logTrace' ::
  EmuCode -> Machine t -> MachineState -> Int ->
  (MachineState,[EmuLog])
logTrace' code preTick s n = (s',w) where
  (((), w), s') = runState (runWriterT $ unEmuMachine m) s
  m = sequence_ $ replicate n $ preTick >> tick code

-- Chunked infinite run.  If there is no log output, this diverges.
logTrace code pre = next where
  next s = w <> next s where
    (s', w) = logTrace' code pre s' 1




machineInit = machineInit0 []
machineInit0 = machineInit' 0

machineInit' :: Int -> [Int] -> MachineState
machineInit' init regs = Map.fromList $
  [(PCounter, 0), (Time, 0)] ++ [(File r, init) | r <- regs]

