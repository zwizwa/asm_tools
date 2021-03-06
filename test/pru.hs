
-- The initial "itch" to write this code was to provide scaffolding
-- for developing PRU code that requires exact instruction timing.  It
-- was clear the final code would be of the woven, unrolled kind.  It
-- seemed like a good idea to make it work in the abstract first,
-- before validating it on the device.  The library was designed with
-- the following requirements in mind:
--
-- 1. Provide a way to represent the standard assembly language
--    exactly.  This is a macro assembler, not a high level
--    programming language.
--
-- 2. Use this representation to generate an emulator that can be
--    extended with instrumentation to compute properties of the
--    program, e.g. state traces, I/O timing diagrams.
--
-- The implementation uses a "tagless final" embedding: a type class
-- Pru is used to abstract the meaning of the embedding by way of
-- abstracting over a compiler monad.  Two versions of this monad are
-- implemented:
--
-- PruGen generates standard assembly text output
-- PruEmu generates an extendable emulator
--
-- Being embedded in Haskell allows ordinary Haskell function
-- composition to be used to write code generators (macros) and
-- validation tests.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Language.Pru               -- Abstract language
import Language.Pru.Gen           -- Concrete assembly text generator
import Language.Pru.Emu           -- Compile to emulator functions
import Language.Pru.BeagleLogic   -- Example code to interface with BeagleLogic firmware
import Language.Pru.Lib

import Data.List
import Data.Map.Strict (Map, lookup, empty, insert, fromList, adjust)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Writer


type EmuOp = Emu ()


main = do
  test_coroutine
  test_beaglelogic_loop
  test_memory

test_coroutine = do
  putStrLn "--- test_coroutine"
  print $ asm coroutine
  let (tick, labels) = compile' (coroutine :: Src)
  print $ labels
  print $ take 30 $ vartrace1 tick (machineInit0 [10,11] []) PCounter

test_beaglelogic_loop = do
  putStrLn "--- test_beaglelogic_loop"
  print $ asm beaglelogic_loop

  let tick = compile beaglelogic_loop
      s0 = machineInit0 [0..31] []
  print $ take 200 $ vartrace1 (gpi >> tick) s0 PCounter


printl es = sequence_ $ map print es
  
-- Run time state variable trace
vartrace :: EmuOp -> EmuState -> [EmuVar] -> [[Int]]
vartrace tick s0 mach_vars = map select trace where
  trace = stateTrace tick s0
  select ms = [ms ! v | v <- mach_vars]

(!) m k = case Map.lookup k m of
  Just v -> v
  Nothing -> error $ "lookup failed: " ++ show k

-- Single
vartrace1 :: EmuOp -> EmuState -> EmuVar -> [Int]
vartrace1 tick s0 mach_var =
  map head $ vartrace tick s0 [mach_var]


-- Emulate GPI events by modifying register R31
gpi :: EmuOp
gpi = modify $ \s -> Map.insert (File 31) (s ! Time) s


-- An example of "macro assembler" use.
initRegs :: Pru m => m ()
initRegs = sequence_ $ [ ldi (R r) (I 0) | r <- [0..31] ]
  


-- Code implementing coroutines through mutual JAL instructions.
-- Correctness can be verified by computing a trace of PCounter.
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


-- Beaglelogic PRU1 data acquisition loop woven with coroutine call.

beaglelogic_loop :: forall m. Pru m => m ()
beaglelogic_loop = do
  
  initRegs

  -- note: Symbolic label names are avoided in the embedding Haskell code.
  -- Pro: identifiers behave better than strings
  -- Con: generated code will have non-descript labels
  -- To alleviate, comments can be inserted in assembly.

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


test_memory = do
  test_memory1
  test_memory2

test_memory1 = do
  putStrLn "--- test_memory1"
  let prog = do
        ldi (Rw 0 0) (I 0)
        ldi (Rw 0 1) (I 1)
        lbbo (R 1) (R 0) (Im $ I 0) (Im $ I 8)
        sbbo (R 1) (R 0) (Im $ I 4) (Im $ I 8)
        -- ldi (Rb 0 1) (I 1)
      (tick, _) = compile' prog
      s0 = Map.fromList (
        [(PCounter, 0), (Time, 0), (File 1, 0), (File 2, 0), (File 0, 123)] ++
        [(Mem addr, val) | (val,addr) <- zip [1,2,3,4,5,6,7,8] [0x10000..]])
        
  print $ asm prog
  printL $ take 5 $ vartrace tick s0 [PCounter, File 1, File 2, File 0, Mem 0x10004]

test_memory2 = do
  putStrLn "--- test_memory2"
  let prog = do
        ldi (R 0) (I 0)
        ldi (Rw 1 0) (I 0x0201)
        ldi (Rw 1 1) (I 0x0403)
        sbbo (R 1) (R 0) (Im $ I 0) (Im $ I 4)
        
      (tick, _) = compile' prog
      s0 = machineInit0 [0..31] [0..3]
        
  print $ asm prog
  printL $ take 5 $ vartrace tick s0 $ [PCounter] ++ map File [0,1] ++ map Mem [0,1,2,3]


printL = traverse print
  
