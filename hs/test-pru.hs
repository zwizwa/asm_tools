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

import Pru
import PruGen
import PruEmu
import BeagleLogic

import Data.List
import Data.Map.Strict as Map

main = do
  putStrLn "PruGen:" >> (print $ asm test1)
  putStrLn "PruEmu:" >> (print $ take 200 $ test_emu)

test_emu = Prelude.map select trace where
  trace = stateTrace machineInit test_io (compile test1)
  select ms = ms ! PCounter

test_io s =
  Map.insert (File 31) (s ! Time) s
  
  
machineInit :: MachineState
machineInit = Map.fromList [
  (PCounter, 0),
  (Time, 0),
  (File 10, 1),
  (File 11, 2)]


test1 :: forall m. Pru m => m ()
test1 = do
  
  -- Symbolic label names are avoided in the embedding Haskell code.
  -- Pro: identifiers behave better than strings
  -- Con: generated code will have non-descript labels
  -- To alleviate, comments can be inserted in assembly.

  initRegs
  
  comment "bl_weave sample"
  bl_weave (sample :: [m ()])
  comment "End"
    

  
initRegs = sequence_ $ [ ldi (R r) 0 | r <- [0..31] ]


  
  
