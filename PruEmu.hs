{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PruEmu(compile,compile'
             ,Src,Comp,Run
             ,RunState(..),RunVar(..)
             ,runEmu,logTrace,logTrace',stateTrace,tickTrace
             ,pseudo, loadm
             ,machineInit,machineInit0,machineInit'
             ) where

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

-- Code is the compiled form of Src.  Each memory address
-- contains a function that emulates the instruction at that location.

-- PRU Code is represented as a unit value in the compiler monad.
type Src w = Comp w ()

-- Compilation and interpretation monads are parameterized by a logger w.

-- Compilation monad
newtype Comp w t =                      
  Emu {unEmu :: (ReaderT Link (WriterT [CompiledOp w] (State CompState)) t) }
  deriving (Functor, Applicative, Monad,
            MonadState CompState,
            MonadWriter [CompiledOp w],
            MonadReader Link)

type CompState = (LabelNb, Addr, Labels)
type CompiledOp w = Either (Run w ()) (Run w ())
type Link = (LabelNb -> Addr)   -- address resolution
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr

-- Run time interpretation monad.
-- State carries the machine state, a Map of RunVar to Int
-- Writer carries a user-specified trace type
newtype Run w t = Run { unRun :: (WriterT w (State RunState) t) }
  deriving (Functor, Applicative, Monad, MonadWriter w, MonadState RunState)

type RunState = Map RunVar Int
data RunVar
  = File Int  -- register file
  | CFlag     -- carry flag
  | PCounter  -- program counter
  | Time      -- instruction counter
  deriving (Eq,Ord,Show)


-- The result of compilation is a machine tick operation, executing
-- one machine cycle in the Writer,State monad
compile :: Monoid w => Src w -> Run w ()
compile = fst . compile'

compile' :: Monoid w => Src w -> (Run w (), Labels)
compile' m = (boot code, labels)  where
  s0 = (0, 0, empty)
  (((), w), s) = runState (runWriterT (runReaderT (unEmu m) r)) s0
  w' = mergePre w
  code = fromList $ zip [0,1..] w'
  (_, _, labels) = s
  r = (labels !) -- circular

-- Normal machine cycle.
boot code = do
  pc <- gets (! PCounter)     -- Read program counter from state
  code ! pc                   -- Run instruction, which updates PCounter
  modify $ adjust (+ 1) Time  -- FIXME: Assumes 1 cycle / instruction

-- The compiler produces either pseudo (Left) or real (Right)
-- instructions.  Pseudo instructions do not consume memory or cycle
-- time, and are prefixed to the first real instruction following
-- it. This behavior is similar to breakpoints. Peudo instructions
-- support instrumentation to perform state modification and trace
-- writing.
mergePre :: Monoid w => [CompiledOp w] -> [Run w ()]
mergePre = f0 where
  f0 = f $ return ()
  f pre [] = [pre] -- insert pseudo instruction at the end
  f pre ((Right m):ops) = (pre >> m) : (f0 ops)
  f pre ((Left  m):ops) = f (pre >> m) ops

-- Compile a (pseudo) instruction
comp   ins = do tell [Right ins] ; modify $ appAddr (+ 1)
pseudo ins = tell [Left ins]

-- Compilation state access
labelNb :: Monoid w => Comp w LabelNb
labelNb = get >>= \(n,_,_) -> return n

addr :: Monoid w => Comp w Addr
addr    = get >>= \(_,a,_) -> return a

appLabelNb f (n,a,l) = (f n, a, l)
appAddr    f (n,a,l) = (n, f a, l)
appLabels  f (n,a,l) = (n, a, f l)

-- Compile time resolution.
-- Label resolution is not in the evaluation path that computes the
-- table.  This enables circular progamming to be used.
link (Im (L l)) = do a <- ask ; return $ Im (I (a l))
link o          = return $ o

-- Run time state access
storem :: Monoid w => RunVar -> Int -> Run w ()
storem var val = do
  modify $ insert var val

loadm :: Monoid w => RunVar -> Run w Int
loadm var = do
  maybe <- gets $ (Map.lookup var)
  return $ checkVar var maybe

checkVar var (Just val) = val
checkVar var Nothing = error $ "Uninitialized RunVar: " ++ show var

-- Registers are special: they have word, byte subaccess.
load :: Monoid w => R -> Run w Int
load (R r)    = loadm (File r)
load (Rw r w) = word 16 w <$> (loadm $ File r)
load (Rb r b) = word  8 b <$> (loadm $ File r)

mask bits = (shift 1 bits) - 1
word bits w v = v' .&. (mask bits) where
  v' = shift v $ 0 - bits

trunc bits = (.&. (mask bits))
  
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

-- Adjust state to resume at the next instruction
next :: Monoid w => Run w ()
next = modify $ adjust (+ 1) PCounter

-- 2-stage dereference of operand to Int.
comp_link_ref f o = do
  o' <- link o -- compile time lookup: label -> addr
  comp $ do
    o'' <- ref o' -- run time lookup: reg -> value
    f o''

-- Generic move.  On PRU this is split into two instructions: MOV and
-- LDI, instead of one instruction that can take multiple operand
-- types.
move ra = comp_link_ref $ \b -> do
  store ra b
  next

-- Generic 2-operand Integer operations.
intop2 :: Monoid w => (Int -> Int -> Int) -> R -> R -> O -> Comp w ()
intop2 f ra rb c = do
  c' <- link c
  comp $ do
    b'  <- ref $ Reg rb
    c'' <- ref c'
    store ra $ f b' c''
    next


-- Pru source code semantics
instance Monoid w => Pru (Comp w) where

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







-- Execution


-- Machines behavior is fully determined by the code represented as a
-- tick operation produced by the compiler, and the initial machine
-- state.
machineInit = machineInit0 []
machineInit0 = machineInit' 0

machineInit' :: Int -> [Int] -> RunState
machineInit' init regs = Map.fromList $
  [(PCounter, 0), (Time, 0)] ++ [(File r, init) | r <- regs]


-- Generic run method.  See below for usage examples.
runEmu :: Monoid w => Run w t -> RunState -> ((t, w), RunState)
runEmu m s = runState (runWriterT $ unRun m) s



-- Two example mechanism are provided to produce traces.

-- 1) Per-instruction traces
--
-- Run machine with custom per-tick tracer.  Machine runs indefinitely
-- producing a lazy stream.  To emulate external influence and have
-- this produce a stream of values, compose the 'tick' operation
-- obtained from 'compile' with pre- and/or post-ops to produce the
-- desired machine manipulation and trace readout.

tickTrace tick s = seq where
  ((seq, _), _) = runEmu m s 
  m = sequence $ cycle [tick]
  
-- Example: infinite machine state trace.
stateTrace tick =
  tickTrace $ do s <- get ; tick ; return s


-- 2) Custom writer traces
--
-- The Writer allows to use 'tell' in pseudo ops to produce
-- user-defined trace types.  Note that to produce a Writer output, we
-- cannot run the machine with an infinite program as above, as the
-- writer result is only available after the computation has finished.
--
-- Therefore, the operation is split into two components: one that
-- runs the machine for a specific amount of cycles,
logTrace' tick s n = (s',w) where
  ((_, w), s') = runEmu m s
  m = sequence_ $ replicate n $ tick

-- and one that produces the infinite stream by repeatedly running one
-- tick.  Note that if there is no log output, this diverges.
logTrace tick = next where
  next s = w <> next s' where
    (s', w) = logTrace' tick s 1


