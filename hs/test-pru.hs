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

import Pru
import PruGen
import PruEmu
import Data.Map.Strict as Map

main = do
  putStrLn "PruGen:" >> (print $ asm test1)
  putStrLn "PruEmu:" >> (print $ take 7 $ test_emu)

test_emu :: [(Int,Int)]
test_emu = Prelude.map select trace where
  trace = stateTrace machineInit (test1 :: EmuProg)
  select ms =
    (ms ! PCounter,
     ms ! R 10)
  
machineInit :: MachineState
machineInit = Map.fromList [
  (PCounter, 0),
  (R 10, 1),
  (R 11, 2)]
  


test1 = do
  -- Symbolic label names are avoided in the embedding Haskell code.
  -- Pro: identifiers behave better than strings
  -- Con: generated code will have non-descript labels

  -- For forward references, a separate declaration step is needed...
  l2 <- declare

  -- .. while label' combines declare and label in case it is not.
  l1 <- label'
  ldi 10 1
  jmp l2

  
  halt -- not reached

  label l2
  ldi 10 2
  jmp l1

test2 = do

  preroll <- label'
  mov 10 11
  mov 11 10

  loop <- label'
  mov 10 11
  mov 11 10
  jmp loop

  
 
    
  
  
  
