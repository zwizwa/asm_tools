{-# LANGUAGE NoMonomorphismRestriction #-}

import Pru
import Data.Map.Strict as Map

main = do
  putStrLn "As IO:"
  test
  putStrLn "As Emulator:"
  
  print $ take 3 $ trace test machineInit
  
machineInit :: Pru.MachineState
machineInit = Map.fromList [
  (Pru.PC,0),
  (Pru.StateR 10,1),
  (Pru.StateR 11,2)]
  


-- Let's make a more concrete test.  I really want 100% control over
-- instruction placement.  This means the end result is a single flat
-- stream of instructions with labels.

-- I don't want to have to deal with label names.  Haskell identifiers
-- should be used.

-- To "bind" other code, just use parameterized functions.
  
test = do

  -- Jump targets need to be declared before use to support forward
  -- references, so bind them to Haskell identifiers.
  preroll <- label
  loop <- label

  -- "blocks" defines a collection of contiguous basic blocks.  A
  -- basic block is a piece of code with a single entry point,
  -- corresponding to an assembly label + instructions.  A block
  -- either falls into the next one, or terminates at a jump
  -- instruction.
  block preroll
  mov r10 r11
  mov r11 r10

  block loop
  mov r10 r11
  mov r11 r10 
  mov r10 r11
  mov r11 r10
  mov r10 r11
  mov r11 r10
  jmp loop
    
  
  
  
