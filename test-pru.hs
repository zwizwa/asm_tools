-- The initial "itch" to write this code was to provide scaffolding
-- for writing code that requires exact instruction timing, where high
-- level validation consists of computing properties of timing
-- diagrams.

-- The PRU's single-cycle deterministic timing makes it possible to
-- implement this, and also makes it easier to emulate the behavior,
-- e.g. to make it work in the abstract first, before running it on
-- the device.

-- At this point, the code does not aim to be a complete emulator.
-- Only the subset of machine behavior used in the target program is
-- implemented.

-- Requirements:
-- 1. Generate assembly language with complete control
-- 2. Emulate execution, extracting signal traces

-- The embedding used is "tagless final", e.g. using a type class
-- layer to be able to give multiple interpretations to the code.  The
-- two requirements above are implemented by two instances of this
-- type class: a macro preprocessor generating flat assembly code to
-- be passed to the TI tools, and an emulator capable of generating
-- machine state traces.

-- Note that this allows ordinary Haskell function composition to be
-- used to write code generators (macros), and validation tests.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Pru
import PruGen
import PruEmu
import BeagleLogic

import Data.List
import Data.Map.Strict (Map, (!), lookup, empty, insert, fromList, adjust)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Writer


-- Use a concrete logger in all tests below
type Log = [Char]
type EmuSrc'   = Src Log      -- emulator source
type EmuComp'  = Comp Log     -- main emulator compiler monad
type EmuRunOp' = Run Log ()   -- run time emulator operation


main = do
  test_coroutine
  test_beaglelogic_loop
  test_logger

test_coroutine = do
  putStrLn "--- run_test_coroutine"
  print $ asm coroutine
  let (tick, labels) = compile' (coroutine :: EmuSrc')
  print $ labels
  print $ take 30 $ mvtrace1 tick (machineInit' 123 [10,11]) PCounter

test_logger = do
  let log = pseudo $ do
        t <- loadm Time
        tell $ "sample: " ++ show t ++ "\n"
      sample' = map (log >>) (sample :: [EmuSrc'])
      src = do
        initRegs
        bl_weave sample'
        return ()
      tick = compile src
      (_, str) = logTrace' tick (machineInit' 123 [10,11]) 60
  putStrLn "log:" >> putStr str
  


printl es = sequence_ $ map print es
  

test_beaglelogic_loop = do
  putStrLn "--- test1" ; print $ asm beaglelogic_loop

  let tick = compile beaglelogic_loop
  print $ take 200 $ mvtrace1 tick machineInit PCounter





-- MachineVar trace
mvtrace :: EmuRunOp' -> RunState -> [RunVar] -> [[Int]]
mvtrace tick s0 mach_vars = Prelude.map select trace where
  trace = stateTrace tick s0
  select ms = [ms ! v | v <- mach_vars]

-- Single
mvtrace1 tick s0 mach_var =
  Prelude.map head $ mvtrace tick s0 [mach_var]


-- Some arbitrary input
pru_input s =
  Map.insert (File 31) (s ! Time) s



beaglelogic_loop :: forall m. Pru m => m ()
beaglelogic_loop = do
  
  -- Symbolic label names are avoided in the embedding Haskell code.
  -- Pro: identifiers behave better than strings
  -- Con: generated code will have non-descript labels
  -- To alleviate, comments can be inserted in assembly.

  initRegs
  

  comment "coroutine instruction pointer init"
  comment "R10: beaglelogic sampler"
  comment "R11: custom control, delay loop"
  entry_10 <- declare
  entry_11 <- declare
  ldi (R 10) entry_10
  ldi (R 11) entry_11
  
  comment "R10 routine body"
  label entry_11
  jal (R 11) (Reg (R 10))  -- save state in R11, jump to R10
  jmp (Im entry_11)

  comment "R11 routine body"
  comment "bl_weave sample_and_yield"
  label entry_10
  bl_weave (sample_and_yield 10 11 :: [m ()])
  
  comment "End"

initRegs = sequence_ $ [ ldi (R r) (I 0) | r <- [0..31] ]
  

coroutine :: forall m. Pru m => m ()
coroutine = do

  comment "coroutine instruction pointer init"
  loop_10 <- declare
  loop_11 <- declare
  ldi (R 10) loop_10
  ldi (R 11) loop_11

  comment "routine 10 body"
  label loop_10
  jal (R 10) (Reg (R 11))  -- save state in R10, jump to R11
  jmp (Im loop_10)

  comment "routine 11 body"
  label loop_11
  jal (R 11) (Reg (R 10))  -- save state in R11, jump to R10
  jmp (Im loop_11)


  
  
