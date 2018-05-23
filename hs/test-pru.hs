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
import Data.List
import Data.Map.Strict as Map

main = do
  putStrLn "PruGen:" >> (print $ asm test1)
  putStrLn "PruEmu:" >> (print $ take 7 $ test_emu)

test_emu :: [(Int,Int)]
test_emu = Prelude.map select trace where
  trace = stateTrace machineInit (test1 :: EmuProg)
  select ms =
    (ms ! PCounter,
     ms ! (File 10))
  
machineInit :: MachineState
machineInit = Map.fromList [
  (PCounter, 0),
  (File 10, 1),
  (File 11, 2)]
  


test1 :: forall m. Pru m => m ()
test1 = do
  -- Symbolic label names are avoided in the embedding Haskell code.
  -- Pro: identifiers behave better than strings
  -- Con: generated code will have non-descript labels

  comment "Mutual References"
  -- For forward references, a separate declaration step is needed...
  l2 <- declare

  -- .. while label' combines declare and label in case it is not.
  l1 <- label'
  ldi (Rb 10 1) 1
  jmp l2

  
  halt -- not reached

  label l2
  ldi (R 10) 2
  jmp l1

  comment "BeagleLogic PRU1 loop"

  loop <- label'
  sequence_ $ interleave [sample :: [m ()], to_pru0 loop]

-- Generate the BeagleLogic PRU1 loop

interleave = concat . transpose

pru1_pru0_interrupt = 0 + 16


-- FIXME: preamble + loop align

to_pru0 :: forall m. Pru m => L -> [m ()]
to_pru0 loop = nops ++ fragment where
  nops = replicate (32 - length fragment) nop
  fragment =
    [add  (R 29) (R 29) (Im 32),      -- Update loop counter
     xout 10 (R 21) 36,               -- Move data across the broadside
     ldi  (R 31) pru1_pru0_interrupt, -- Jab PRU0
     jmp  loop] :: [m ()]

-- Instruction sequence for sampler
sample = do
  r <- [21..28]
  b <- [0,1,2,3]
  return $ mov (Rb r b) (Rb 31 0)



-- FIXME: can SET be used instead of LDI to jab PRU0 while still using
-- only one cycle?



 
    
  
  
  
