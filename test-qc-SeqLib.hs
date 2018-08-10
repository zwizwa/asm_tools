-- RTL code tends to be "algebraic".  I.e. all the action happens
-- within the domain of a type.  Apart from enforcing basic code
-- structure, types do not help.  Tests are needed to constrain
-- behavior.
-- 
-- QuickCheck is used to raise the abstraction level of tests.
-- However, do note that in practice it is easier to work in steps,
-- i.e. graduate from "there exists" to "for all":
--
-- . Create a trace wrapper (t_) to transform the abstract (r S)
--   functions into the Int domain.
--
-- . From the trace wrapper, reate a single example (x_), that prints
--   out a human-readable report about how the function is performing.
--
-- . Once this works, generalize the example into property tests (p_)
--   to add test coverage, and comment out the verbose example.


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Seq
import SeqLib
import SeqEmu
import SeqPrim
import TestSeqLib
import TestTools

import qualified SeqTH 

import Data.Char
import Data.Bits
import Data.List
import Data.List.Split
import Test.QuickCheck hiding ((.&.),(.|.))
import Test.QuickCheck.Gen hiding (bitSize, getLine)
import Language.Haskell.TH


main = do

  -- print $ toWord [1,0,0,0]
  -- print $ toBitList 4 8
  -- print $ downSample' $ t_clocked_shift 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0,0,0,0,1]]

  -- x_async_receive_sample_emu
  -- x_async_receive_sample_th
  x_th_async_receive
  x_async_receive_emu
  x_async_receive_th
  x_mem
  x_fifo
  
  qc "p_bits" p_bits
  qc "p_sample" p_sample
  qc "p_clocked_shift" p_clocked_shift
  qc "p_async_receive" p_async_receive
  qc "p_fifo" p_fifo

  x_cpu_ins
  x_stack
  x_async_transmit

-- Tests for library code.
--    t_  Trace wrapper (_emu or _th)
--    e_  Higher level test evaluator  fst=bool
--    x_  Example printout
--    p_  Property test

--    _emu SeqEmu code.  Slow, but flexible.
--    _th  TH code. Fast, but only pure target semantics.

-- clocked_shift

p_clocked_shift = forAll vars pred where
  vars = do
    sub <- choose (1,8)
    wl  <- wordList
    return (sub, wl)
  pred (sub, (nb_bits, words)) = words == words' where
    bits   = toBits nb_bits words
    ins    = upSample' (cycle [sub]) $ map (:[]) bits
    outs   = t_clocked_shift nb_bits ins
    words' = map head $ downSample' outs

t_clocked_shift :: Int -> [[Int]] -> [[Int]]
t_clocked_shift nb_bits = trace [1,1] $ \[bc,bv] -> do
  (wc, wv) <- clocked_shift ShiftLeft (SInt (Just nb_bits) 0) (bc, bv)
  return [wc, wv]


-- async_receive_sample

t_async_receive_sample_emu nb_bits = trace [1] $ \[i] -> do
  d_async_receive_sample nb_bits i

t_async_receive_sample_th nb_bits@8 =
  $(SeqTH.compile [1] $ \[i] -> d_async_receive_sample 8 i) memZero

x_async_receive_sample_emu = do
  putStrLn "-- x_async_receive_sample_emu"
  x_async_receive_sample_for t_async_receive_sample_emu

x_async_receive_sample_th = do
  putStrLn "-- x_async_receive_sample_th"
  x_async_receive_sample_for t_async_receive_sample_th
  
x_async_receive_sample_for t = do
  printC $ chunksOf 8 $ t 8
    [[i] | i <- rle (1,[8,8*9,8])]


-- async_receive

t_async_receive_emu nb_bits = trace [1] $ \[i] ->
  d_async_receive nb_bits i

t_async_receive_th nb_bits@8 =
  $(SeqTH.compile [1] $ \[i] -> d_async_receive 8 i) memZero

x_th_async_receive = do
  putStrLn "-- x_th_async_receive"
  putStrLn $ pprint $ SeqTH.compile' [1] $ \[i] -> d_async_receive 8 i

x_async_receive_emu = do
  putStrLn "-- x_async_receive_emu"
  x_async_receive_for t_async_receive_emu

x_async_receive_th = do
  putStrLn "-- x_async_receive_th"
  x_async_receive_for t_async_receive_th

x_async_receive_for t = do
  let
    -- ins = map ord "ABCDEF"
    ins = [0,7..255]
    out = t 8 $ map (:[]) $ uartBits 8 ins
    sample [v,i,bc,wc@1,state,count] = Just v
    sample _ = Nothing
    out' = downSample sample out
  -- printC $ chunksOf 8 out
  print $ out'

p_async_receive = forAll (listOf $ word 8) pred where
  pred words = words == words' where
    ins    = map (:[]) $ uartBits oversample words
    outs   = t_async_receive_th nb_bits $ ins  -- th is a lot faster
    words' = downSample sample outs
    sample [v,i,bc,wc@1,state,count] = Just v
    sample _ = Nothing
  
  oversample = 8
  nb_bits = 8


-- async_receive
t_async_transmit =
  $(SeqTH.compile [1,1,8] d_async_transmit) memZero
  
x_async_transmit = do
  let ins = take 25 $ [[0,0,0],[0,0,0],[1,1,0x5A]] ++ (cycle $ [[0,0,0],[1,0,0]])
  putStrLn "-- x_async_transmit"
  printL $ t_async_transmit ins
  putStrLn $ pprint $ SeqTH.compile' [1,1,8] d_async_transmit

-- mem

t_mem = trace [8,1,8,8] $ \i@[ra,we,wa,wd] -> do
  t <- stype wd
  closeMem [t] $ \[rd] ->
    return ([(we, wa, wd, ra)], (rd:i))

x_mem = do
  let writes = [[0,1,x,x+20] | x <- [1..10]]
      reads  = [[x,0,0,0]    | x <- [1..10]]
      outs = t_mem $ writes ++ reads
  putStrLn "-- x_mem rd,ra,we,wa,wd"
  printL outs


-- fifo  (d_fifo is in TestSeqLib.hs to allow staging)

-- t_fifo_emu = trace [1,1,8] d_fifo
t_fifo = $(SeqTH.compile [1,1,8] d_fifo) memZero

e_fifo lst = (lst == lst', (lst',outs)) where
  lst'   = map head $ downSample' outs
  -- Write data into the buffer, read it out.
  outs   = t_fifo $ writes ++ reads ++ idle
  writes = [[0,1,x] | x <- lst]
  reads  = replicate (length lst) $ [1,0,0]
  idle   = replicate  3           $ [0,0,0]

x_fifo = do
  let lst = [1..10]
      (_, (lst', outs)) = e_fifo lst
  putStrLn "-- x_fifo rd,wa,ra,re,we,wd"
  printL outs
  print lst
  print lst'

p_fifo = forAll (listOfMaxSize 16 $ word 8) (fst . e_fifo)


-- Stack

t_stack = $(SeqTH.compile [1,1,8] d_stack) memZero

x_stack = do
  let pushes = [[1,0,n] | n <- [1..10]]
      pops   = [[0,1,0] | n <- [1..10]]
      nop    = [[0,0,0]]
  putStrLn "-- stack"
  printL $ t_stack $ pushes ++ nop ++ pops ++ nop
  

-- CPU

memRef :: [Int] -> Int -> Int
memRef mem n = v where
  v = case (n >= length mem) of
    True -> 0
    False -> mem !! n

t_cpu_ins = $(SeqTH.compile [] d_cpu_ins) [iMemInit] where
  iMemInit = memRef iMem
  iMem = [ 0x8004,    -- jump 4
           0, 0, 0,   -- nop
           0x8000 ]   -- jump 0

x_cpu_ins = do
  putStrLn "-- x_cpu_ins"
  printL $ t_cpu_ins $ replicate 10 []




-- Tools tests

-- downSample is left inverse of upSample
-- Test this separately as it is used in other tests.
p_sample = forAll vars pred where
  vars = sized $ \n -> do
    spaces <- vectorOf n $ choose (1,8)
    seq    <- vectorOf n $ arbitrary
    return (spaces, seq)
  pred (spaces, seq) = seq == seq' where
    seq' = downSample' $ upSample' spaces seq



p_bits = forAll wordList p where
  p (nb_bits, words) = p1 && p2 where
    
    p1 = words == words'
    p2 = bits  == bits'
  
    bits   = toBits  nb_bits words
    words' = toWords nb_bits bits
    bits'  = toBits  nb_bits words'
  
  
  
