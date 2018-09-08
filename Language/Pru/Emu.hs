-- (c) 2018 Tom Schouten -- see LICENSE file

-- Note: This is experimental code driven by a concrete need.
-- Not all corner cases are implemented or implemented correctly.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pru.Emu(
  compile,compile'
  ,Src,Comp,Emu
  ,EmuState(..),EmuVar(..)
  ,runEmu,stateTrace,tickTrace
  ,pseudo, loadm
  ,machineInit,machineInit0,machineInit'
  ,rle
  ) where

import Language.Pru

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

-- PRU assembly code is unit value in the compiler monad.
type Src = Comp ()

-- Compilation and interpretation monads are parameterized by a logger w.

-- Compilation monad
newtype Comp t =                      
  Comp {unComp :: (ReaderT Link (WriterT [CompiledOp] (State CompState)) t) }
  deriving (Functor, Applicative, Monad,
            MonadState CompState,
            MonadWriter [CompiledOp],
            MonadReader Link)
data CompiledOp = PseudoOp (Emu ()) | RealOp (Emu ())
type CompState = (LabelNb, Addr, Labels)
type Link = (LabelNb -> Addr)   -- address resolution
type LabelNb = Int
type Addr = Int
type Labels = Map LabelNb Addr

-- Run time interpretation monad.
-- State carries the machine state, a Map of EmuVar to Int
-- Writer carries a user-specified trace type
newtype Emu t = Emu { unEmu :: State EmuState t }
              deriving (Functor, Applicative, Monad,
                        MonadState EmuState)


type EmuState = Map EmuVar Int
data EmuVar
  = File Int  -- 32 bit register file
  | Mem Int   -- byte addressed memory
  | CFlag     -- carry flag
  | PCounter  -- program counter
  | Time      -- instruction counter
  deriving (Eq,Ord,Show)


-- The result of compilation is a machine tick operation, executing
-- one machine cycle in the Writer,State monad
compile :: Src -> Emu ()
compile = fst . compile'

compile' :: Src -> (Emu (), Labels)
compile' m = (boot code, labels)  where
  s0 = (0, 0, empty)
  (((), w), s) = runState (runWriterT (runReaderT (unComp m) r)) s0
  w' = mergePseudoOps w
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
mergePseudoOps :: [CompiledOp] -> [Emu ()]
mergePseudoOps = f0 where
  f0 = f $ return ()
  f pre [] = [pre] -- insert pseudo instruction at the end
  f pre ((RealOp m):ops) = (pre >> m) : (f0 ops)
  f pre ((PseudoOp m):ops) = f (pre >> m) ops

-- Compile a (pseudo) instruction
comp   ins = do tell [RealOp ins] ; modify $ appAddr (+ 1)
pseudo ins = tell [PseudoOp ins]

-- Compilation state access
labelNb :: Comp LabelNb
labelNb = get >>= \(n,_,_) -> return n

addr :: Comp Addr
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
storem :: EmuVar -> Int -> Emu ()
storem var val = do
  modify $ insert var val

loadm :: EmuVar -> Emu Int
loadm var = do
  maybe <- gets $ (Map.lookup var)
  return $ checkVar var maybe

checkVar var (Just val) = val
checkVar var Nothing = error $ "Uninitialized EmuVar: " ++ show var

-- Registers are special: they have word, byte subaccess.

-- FIXME: is 'word' actually correct?

load :: R -> Emu Int
load (R r)    = loadm (File r)
load (Rw r w) = word 16 w <$> (loadm $ File r)
load (Rb r b) = word  8 b <$> (loadm $ File r)

mask bits = (shift 1 bits) - 1
word bits w v = v' .&. (mask bits) where
  v' = shift v $ 0 - bits

trunc bits = (.&. (mask bits))
  
store (R r) = (storem $ File r) . (trunc 32)
store (Rw r w) = store' 16 w (R r)
store (Rb r b) = store'  8 b (R r)


-- This is an awkward operation, so factor it out.
merge_bits bit_size bit_offset old sub = new where
  m = mask bit_size
  erased = old    .&. (complement $ m `shiftL` bit_offset)
  new    = erased .|. ((sub .&. m) `shiftL` bit_offset)

-- This is really awkward to express!  Why?
store' bits index (R r) sub = do
  old <- loadm (File r)
  storem (File r) $ merge_bits bits (bits * index) old sub

clrbit val bit = val .&. (complement $ shift 1 bit)
setbit val bit = val .|. (shift 1 bit)

-- Generic run time operand dereference.
ref (Im (I im))  = return im
ref (Im (L l))   = error $ "label " ++ show l ++ " not resolved"
ref (Reg r)      = load r

-- Adjust state to resume at the next instruction
next :: Emu ()
next = modify $ adjust (+ 1) PCounter

-- 2-stage dereference of operand to Int.
comp_link_ref f o = do
  o' <- link o -- compile time lookup: label -> addr
  comp $ do
    o'' <- ref o' -- run time lookup: reg -> value
    f o''


-- Contitional relative branches.
cond :: OpcIRO -> Int -> Int -> Bool
cond QBNE = (/=)
cond QBEQ = (==)

cond_branch p l r o = do
  case l of
    L _ -> return ()
    I _ -> error "literal relative jumps not supported"

  l' <- link $ Im l
  comp $ do
    l'' <- ref l'
    r'  <- ref $ Reg  r
    o'  <- ref o
    case p  r' o' of
      True  -> storem PCounter l''
      False -> next


-- Generic move.  On PRU this is split into two instructions: MOV and
-- LDI, instead of one instruction that can take multiple operand
-- types.
move ra = comp_link_ref $ \b -> do
  store ra b
  next

-- Generic 2-operand Integer operations.
intop2 :: (Int -> Int -> Int) -> R -> R -> O -> Comp ()
intop2 f ra rb c = do
  c' <- link c
  comp $ do
    b'  <- ref $ Reg rb
    c'' <- ref c'
    store ra $ f b' c''
    next

-- Load and store behave as memcpy between the addressable memory and
-- the register file.  Note that we do not have accurate timing
-- information for these, so use them only for setup and debugging.

sbbo' = bbo $ \reg_addr mem_addr -> do
  byte <- load (addr2reg reg_addr)
  storem (Mem mem_addr) byte

lbbo' = bbo $ \reg_addr mem_addr -> do
  byte <- loadm $ Mem mem_addr
  store (addr2reg reg_addr) byte

reg2addr (R r) = 4 * r
reg2addr (Rw r w) = 4 * r + 2 * w
reg2addr (Rb r b) = 4 * r + b

addr2reg addr = (Rb r b) where
  r = addr `shiftR` 2
  b = trunc 2 addr

-- Iteration over memory and register file addresses.
bbo :: (Int -> Int -> Emu ()) -> R -> R -> O -> O -> Comp ()
bbo op reg mem_ptr (Im (I mem_offset)) (Im (I nb)) =  do
  -- FIXME: the last argument can also be R0.bn
  comp $ do
    let reg_addr = reg2addr reg
    mem_addr <- fmap (mem_offset +) $ load mem_ptr
    sequence_ [ op (reg_addr + i) (mem_addr + i) | i <- [0..nb-1] ]
    next


-- Pru source code semantics
instance Pru Comp where

  declare = do
    n <- labelNb
    modify $ appLabelNb (+ 1)
    return $ L n
    
  label (L l) = do
    a <- addr
    modify $ appLabels $ \ls -> insert l a ls

  inso JMP = comp_link_ref $ storem PCounter
  insi QBA l = inso JMP $ Im l -- FIXME: only labels!
  insiro op l r o = cond_branch (cond op) l r o
    
  insro JAL (R r) = comp_link_ref $ \o -> do
      pc <- loadm PCounter
      storem (File r) (pc + 1)
      storem PCounter o

  insrroo SBBO = sbbo'
  insrroo LBBO = lbbo'
    
  -- Generic instructions
  insrr MOV ra rb = move ra (Reg rb)
  insri LDI ra ib = move ra (Im ib)
  insrro ADD = intop2 (+)
  insrro SUB = intop2 (-)
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

machineInit' :: Int -> [Int] -> EmuState
machineInit' init regs = Map.fromList $
  [(PCounter, 0), (Time, 0)] ++ [(File r, init) | r <- regs]


-- Generic monad run method.  See below for usage examples.
runEmu :: Emu t -> EmuState -> (t, EmuState)
runEmu m s = runState (unEmu m) s



-- Two example mechanism are provided to produce traces.

-- 1) Per-instruction traces
--
-- Run machine with custom per-tick tracer.  Machine runs indefinitely
-- producing a lazy stream.  To emulate external influence and have
-- this produce a stream of values, compose the 'tick' operation
-- obtained from 'compile' with pre- and/or post-ops to produce the
-- desired machine manipulation and trace readout.

tickTrace tick s = seq where
  (seq, _) = runEmu m s 
  m = sequence $ cycle [tick]
  
-- Example: infinite machine state trace.
stateTrace tick =
  tickTrace $ do s <- get ; tick ; return s


-- 2) Custom writer traces
--
-- FIXME: writer monad removed.  Do this in the state monad.  Pseudo
-- ops can manipulate state.

-- FIXME: Something to consider. It might be simpler to extend the
-- main state with some existential types, remove the writer layer,
-- and implement a different writer abstraction on top of the state
-- layer.  This would also make it possible to make an "infinite"
-- writer.



-- Misc emulation tools

-- Run length encoding
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (a : as) = f (a, 1) as where
  f s [] = [s]
  f s@(a', n) l@(a : as) = case a' == a of
    True  -> f (a', n+1) as
    False -> (s : rle l)


