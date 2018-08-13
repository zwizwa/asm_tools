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
import CPU
import SeqEmu
import SeqPrim
import TestSeqLib
import TestTools

import SeqTH(compile,compile',noProbe,allProbe)

import Prelude hiding (read, drop)
import Data.Char
import Data.Bits
import Data.List hiding (drop)
import Data.List.Split
import Data.Bits
import Data.Maybe
import Test.QuickCheck hiding ((.&.),(.|.))
import Test.QuickCheck.Gen hiding (bitSize, getLine)
import Language.Haskell.TH


main = do

  -- print $ toWord [1,0,0,0]
  -- print $ toBitList 4 8
  -- print $ downSample' $ t_deser 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0,0,0,0,1]]

  -- x_async_receive_sample_emu
  -- x_async_receive_sample
  -- x_async_receive_emu
  x_th_async_receive
  x_async_receive
  x_mem
  x_fifo
  
  qc "p_bits" p_bits
  qc "p_sample" p_sample
  qc "p_deser" p_deser
  qc "p_async_receive" p_async_receive
  qc "p_spi" p_spi
  qc "p_fifo" p_fifo

  x_stack
  x_async_transmit
  x_spi
  x_soc
  

-- Tests for library code.
--    t_  Trace wrapper (_emu or _th)
--    e_  Higher level test evaluator  fst=bool
--    x_  Example printout
--    p_  Property test

--    _emu SeqEmu code.  Slow, but flexible.
--    _th  TH code. Fast, but only pure target semantics.

-- deser

p_deser = forAll vars pred where
  vars = do
    sub <- choose (1,8)
    wl  <- wordList
    return (sub, wl)
  pred (sub, (nb_bits, words)) = words == words' where
    bits   = toBits nb_bits words
    ins    = reSample' (cycle [sub]) $ map (:[]) bits
    outs   = t_deser nb_bits ins
    words' = map head $ downSample' outs

t_deser :: Int -> [[Int]] -> [[Int]]
t_deser nb_bits = trace [1,1] $ \[bc,bv] -> do
  (wc, wv) <- deser ShiftLeft (SInt (Just nb_bits) 0) (bc, bv)
  return [wc, wv]


-- async_receive_sample

-- t_async_receive_sample_emu nb_bits = trace [1] $ \[i] -> do
--   async_receive_sample nb_bits i >>= list2

t_async_receive_sample nb_bits@8 ins =
  snd $
  $(compile noProbe [1] $
    \[i] -> async_receive_sample 8 i >>= list2)
  memZero ins

-- x_async_receive_sample_emu = do
--   putStrLn "-- x_async_receive_sample_emu"
--   x_async_receive_sample_for t_async_receive_sample_emu

x_async_receive_sample = do
  putStrLn "-- x_async_receive_sample"
  x_async_receive_sample_for t_async_receive_sample
  
x_async_receive_sample_for t = do
  printC $ chunksOf 8 $ t 8
    [[i] | i <- rle (1,[8,8*9,8])]


-- async_receive

-- t_async_receive_emu nb_bits = trace [1] $ \[i] ->
--   async_receive nb_bits i >>= list2

t_async_receive nb_bits@8 ins =
  snd $
  $(compile allProbe [1] $
    \[i] -> async_receive 8 i >>= list2)
  memZero ins

x_th_async_receive = do
  putStrLn "-- x_th_async_receive"
  putStrLn $ pprint $ compile' noProbe [1] $
    \[i] -> async_receive 8 i >>= list2

-- x_async_receive_emu = do
--   putStrLn "-- x_async_receive_emu"
--   x_async_receive_for t_async_receive_emu

x_async_receive = do
  putStrLn "-- x_async_receive"
  x_async_receive_for t_async_receive

x_async_receive_for t = do
  let
    -- ins = map ord "ABCDEF"
    ins = [0,7..255]
    out = t 8 $ map (:[]) $ uartBits 8 ins
    -- sample [v,i,bc,wc@1,state,count] = Just v
    sample (wc@1:v:_) = Just v
    sample _ = Nothing
    out' = downSample sample out
  -- printC $ chunksOf 8 out
  print $ out'

p_async_receive = forAll (listOf $ word 8) pred where
  pred words = words == words' where
    ins    = map (:[]) $ uartBits oversample words
    outs   = t_async_receive nb_bits $ ins  -- th is a lot faster
    words' = downSample sample outs
    -- sample [v,i,bc,wc@1,state,count] = Just v
    sample (wc@1:v:_) = Just v
    sample _ = Nothing
  
  oversample = 8
  nb_bits = 8


-- async_transmit
t_async_transmit ins =
  snd $
  $(compile noProbe [1,1,8] $
     \[bc,wc,tx] -> async_transmit bc (wc, tx) >>= list2)
  memZero ins

x_async_transmit = do
  let tx_ins = take 100 $ [[0,0,0],[0,0,0],[1,1,0x5A]] ++
               -- 8x oversampling
               (cycle $ replicate 7 [0,0,0] ++ [[1,0,0]])
               
      tx_out = t_async_transmit tx_ins
      -- use the receiver to test the transmitter
      dline  = map head tx_out  -- the line carrying the data
      rx_out = t_async_receive 8 $ map (:[]) dline

  putStrLn "-- x_async_transmit"
  putStrLn "tx:"
  printL $ tx_out
  putStrLn "rx:"
  printL $ rx_out


-- Using SeqTH it is not possible to generate outputs in response to
-- inputs at the test bench level.  This is annoying, but for now not
-- really an issue since it is possible to either go to SeqEmu, or
-- provide feedback at the Seq level.  It seems simplest to start
-- using CPU-like sequencers for that.  See below.


-- spi
t_spi ins =
  $(compile allProbe [1,1,1] $
    \[cs, sclk,sdata] -> do
      sync_receive 8 cs sclk sdata
      return [])
  memZero ins

e_spi bytes  = (bytes == bytes', (bytes', table)) where
  ins    = [[1,0,0]] ++ [[0,c,d] | (c,d) <- spiBits 2 bits] ++ [[1,0,0]]
  bits   = concat $ map (toBitList 8) bytes
  bytes' = map head $ downSample' $ selectSignals ["s_wc","s_w"] probes outs
  table@(probes, outs) = t_spi ins

x_spi = do
  let bytes = [202,123]
      (_, (bytes', table)) = e_spi bytes

  putStrLn "-- x_spi"
  print bytes'
  printProbe ["s_wc","s_w","sclk","sdata","s_bc"] $ table

p_spi = forAll (listOf $ word 8) $ fst . e_spi



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
t_fifo ins = snd $ $(compile noProbe [1,1,8] d_fifo) memZero ins

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

t_stack ins = snd $ $(compile noProbe [1,1,8] d_stack) memZero ins

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

t_soc prog = $(compile allProbe [1] soc_test) [memRef prog]

printProbe :: [String] -> ([String],[[Int]]) -> IO ()
printProbe columns (names, signals) = do
  let signals' = selectSignals columns names signals
  putStr $ showSignals columns signals'

-- For testing, it seems simplest to embed the CPU inside a SOC.  What
-- is important is the integration, not so much the CPU itself, which
-- is fairly straightforward.

x_soc = do
  let
    -- Most basic operation is a jump.
    prog_jmp = [ jmp 4, nop, nop, nop, jmp 0 ]

    -- Stack access
    prog_push = [ push 101, push 102, push 103, drop, jmp 0 ]
    
    -- Uart control: write + wait done, then loop
    prog_bus = [ push 0xF, write 2, read 1, jmp 0 ]

    -- Loop
    prog_loop = [ push 3, loop 1, jmp 0 ]

  putStrLn "-- x_soc"
  
  putStrLn "prog_jmp:"
  printProbe ["iw","ip"] $
    t_soc prog_jmp $ replicate 10 [1]

  putStrLn "prog_push:"
  printProbe ["iw","ip","top","snd"] $
    t_soc prog_push $ replicate 10 [1]

  putStrLn "prog_bus:"
  printProbe ["iw","ip","tx_bc","tx_wc","tx_in","tx_done","tx_out"] $
    t_soc prog_bus $ replicate 30 [1]

  putStrLn "prog_loop:"
  printProbe ["iw","ip","top","snd","c"] $
    t_soc prog_loop $ replicate 30 [1]
  

  





-- A slightly more involved program: read uart data into fifo until
-- newline character, then spit it out again.  Split it into two
-- parts: instruction decoder, state machines.






-- Tools tests

-- downSample is left inverse of upSample
-- Test this separately as it is used in other tests.
p_sample = forAll vars pred where
  vars = sized $ \n -> do
    spaces <- vectorOf n $ choose (1,8)
    seq    <- vectorOf n $ arbitrary
    return (spaces, seq)
  pred (spaces, seq) = seq == seq' where
    seq' = downSample' $ reSample' spaces seq



p_bits = forAll wordList p where
  p (nb_bits, words) = p1 && p2 where
    
    p1 = words == words'
    p2 = bits  == bits'
  
    bits   = toBits  nb_bits words
    words' = toWords nb_bits bits
    bits'  = toBits  nb_bits words'
  
  
  
