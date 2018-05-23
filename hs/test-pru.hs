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
  -- To alleviate, comments can be inserted in assembly.

  test_preroll
  comment "End"
    
  

-- The test case is a weaver for the PRU1 data acquisition loop used
-- in the BeagleLogic.  It consists of two statically interwoven tasks:

-- a) Sample + I/O control
-- b) Transport + loop control

-- The skeleton is the same as in the BeagleLogic, but parameterized
-- to be able to play with it a bit.

-- Essentially, these two loops need to be aligned by creating a
-- preroll of the first loop.

-- As an example, recreate the BeagleLogic 100MHz loop.

test_preroll :: forall m. Pru m => m L
test_preroll = do
  comment "test_preroll"
  again <- declare  -- label used inside loop
  let pre   = 2
      pad   = nops :: Int -> [m()]
      loop1 = sample :: [m()]
      loop2 = transfer (length loop1) again :: [m()]

  entry <- label' -- main entry label
  preroll_zip pre pad again loop1 loop2
  return entry

preroll_zip nb_pre pad again loop1 loop2 = code where
  (pre1, rest) = splitAt nb_pre loop1
  loop1' = rest ++ pre1

  pre  = merge [pre1, pad (length pre1)]
  loop = merge [loop1', loop2]

  code =
    sequence_ pre >>
    label again >>
    sequence_ loop
  
         
-- interleaving merge
merge = concat . transpose
  
  





-- a) I/O control preparing for sample.  This can be a coroutine call.

-- b) sample from R31.b0 to R21 - R29
sample :: Pru m => [m ()]
sample = do
  r <- [21..28]
  b <- [0,1,2,3]
  return $ mov (Rb r b) (Rb 31 0)
  
-- c) loop maintenance + moving data from R21 - R29 across the broadside bus
transfer :: forall m. Pru m => Int -> L -> [m ()]
transfer nb_samples again = fill ++ tail where
  fill =
    nops $ nb_samples - length tail
    :: [m ()]
  tail =
    [xout 10 (R 21) 32,   -- Move data across the broadside
     ldi  (R 31) 36,      -- Interrupt PRU0
     jmp again]
    :: [m ()]
              
nops :: forall m. Pru m => Int -> [m ()]
nops nb = replicate nb nop




-- FIXME: handcode the aquisition loop


 
    
  
  
  
