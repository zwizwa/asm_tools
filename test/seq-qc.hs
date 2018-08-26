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

import Language.Seq
import Language.Seq.Lib
import Language.Seq.CPU
import Language.Seq.Emu
import Language.Seq.Prim
import Language.Seq.Test.Lib  -- for staging
import Language.Seq.Test.Tools

import qualified Language.Seq.Forth as Forth

import Language.Seq.TH(compile,compile',noProbe,allProbe)

import Prelude hiding (read, drop)
import Data.Char
import Data.Bits
import Data.List hiding (drop)
import Data.List.Split
import Data.Bits
import Data.Maybe
import Test.QuickCheck hiding ((.&.),(.|.),again)
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

  qc "p_soc_fun" p_soc_fun

  x_stack
  x_async_transmit
  x_spi
  x_mod_counter
  x_soc
  x_soc_boot
  x_deser

-- Tests for library code.
--    t_  Trace wrapper (_emu or _th)
--    e_  Higher level test evaluator  fst=bool
--    x_  Example printout
--    p_  Property test

--    _emu SeqEmu code.  Slow, but flexible.
--    _th  TH code. Fast, but only pure target semantics.

-- deser

p_deser = forAll vars $ fst . e_deser where
  vars = do
    sub <- choose (1,8)
    wl  <- wordList
    return (sub, wl)

e_deser (sub, (nb_bits, words)) = (words == words', outs) where
    bits   = toBits nb_bits words
    ins    = reSample' (cycle [sub]) $ map (:[]) bits
    outs   = t_deser nb_bits ins
    words' = downSampleCD outs


t_deser :: Int -> [[Int]] -> [[Int]]
t_deser nb_bits = trace [1,1] $ \[bc,bv] -> do
  (wc, wv) <- deser ShiftLeft nb_bits 0 bc bv
  return [wc, wv]

x_deser = do
  putStrLn "-- x_deser"
  let spec1 = (2, (8, [1,2,3]))
      spec  = (4,(10,[0,1023,1018,0,5,4]))  -- from QC fail
      (_, outs) = e_deser spec
  print spec
  print $ downSampleCD outs
  printL' outs
  

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
    [[i] | i <- rle' (1,[8,8*9,8])]


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

t_spi' code ins = code memZero ins
t_spi Mode0 = t_spi' $(compile allProbe [1,1,1] $ d_spi Mode0 8)
t_spi Mode1 = t_spi' $(compile allProbe [1,1,1] $ d_spi Mode1 8)
t_spi Mode2 = t_spi' $(compile allProbe [1,1,1] $ d_spi Mode2 8)
t_spi Mode3 = t_spi' $(compile allProbe [1,1,1] $ d_spi Mode3 8)

e_spi (mode, bytes)  = (bytes == bytes', (bytes', table)) where
  (cpol, cpha) = spi_mode mode
  ins    = [[1,cpol,0]] ++
           [[0,c,d] | (c,d) <- upSample 2 $ spiBits mode bits] ++
           [[1,cpol,0]]

  bits   = concat $ map (toBitList 8) bytes
  bytes' = downSampleCD $ selectSignals ["s_wc","s_w"] probes outs
  table@(probes, outs) = t_spi mode ins

x_spi = do
  let cfg' = (Mode0, [202,123])
      cfg@(_,bytes)  = (Mode1, [4,5])
      (_, (bytes', table)) = e_spi cfg

  putStrLn "-- x_spi"
  print bytes
  print bytes'
  printProbe ["s_wc","s_w","sclk","sdata","s_bc"] $ table

p_spi = forAll vars $ fst . e_spi where
  vars = do
    bytes <- listOf $ word 8
    mode  <- oneof $ map return [Mode0,Mode1,Mode2,Mode3]
    return (mode, bytes)



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
  lst'   = downSampleCD outs
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

-- For testing, it seems simplest to embed the CPU inside a SOC.
-- CPU.hs contains an example SOC.  (Typically, you would want to
-- change the bus + peripheral part of the SOC for your application).

-- Below contains some signal printouts for several programs, and
-- quickcheck properties of programs producing dbg traces.

memRef :: [Int] -> Int -> Int
memRef mem n = v where
  v = case (n >= length mem) of
    True -> 0
    False -> mem !! n

t_soc_idle = [1,1,1,0,0]
t_soc prog = $(compile allProbe [1,1,1,1,1] soc_test) [memRef prog]

printProbe :: [String] -> ([String],[[Int]]) -> IO ()
printProbe columns (names, signals) = do
  let signals' = selectSignals columns names signals
  putStr $ showSignals' True columns signals'

-- Subroutine abstractions.  Note that this is not a 2-stack
-- machine, so the return address needs to be restored to the top
-- of stack before returning.
prog_fun v1 v2 = program $ do
  proc1 <- fun $ do push v1 ; swap ; ret
  proc2 <- fun $ do push v2 ; swap ; ret
  start $ forever $ do
    proc1 ; write dbg
    proc2 ; write dbg

e_soc_fun v1 v2 = e_soc_dbg_trace clocks expect (prog_fun v1 v2) where
  clocks = 40
  expect = take 6 $ cycle [v1, v2]

p_soc_fun = forAll vars prop where
  vars = do
    v1 <- word 8
    v2 <- word 8
    return (v1, v2)
  prop (v1, v2) = fst $ e_soc_fun v1 v2
    

-- Generic program -> dbg trace test.
e_soc_dbg_trace nb_cycles expect prog = (expect == dbg, (dbg, out)) where
  ins = replicate nb_cycles t_soc_idle
  out = t_soc prog ins
  dbg = take (length expect) $ dbg_trace out

x_soc = do
  let
    c = Forth.compile
    -- Most basic operation is a jump.
    prog_jmp =  c $ do jmp 4; nop; nop; nop; jmp 0

    -- Stack access
    prog_push = c $ do push 7; push 9; push 11; swap; drop; jmp 0
    
    -- Uart control: write + wait done, then loop
    prog_bus =  c $ do
      push 0xf
      -- push 0xF
      write uart_tx
      nop            -- tx_done doesnt clear fast enough
      read  uart_tx  -- waits until tx_done is high
      drop           -- value returned is dummy
      jmp 0

    -- Loop
    prog_loop = c $ do push 3; loop 1; jmp 0

    -- Same, using Forth control words
    prog_loop2 = c $ do begin; push 3; for; next; again

    -- Write to debug register
    prog_dbg = c $ do push 123; write dbg

    -- Call, ret
    prog_call = c $ do
      call 4
      call 4
      call 4
      jmp 3
      nop
      ret

    
    idle = t_soc_idle
  
  putStrLn "-- x_soc"
  
  putStrLn "prog_jmp:"
  printProbe ["iw","ip"] $
    t_soc prog_jmp $ replicate 10 idle

  putStrLn "prog_push:"
  printProbe ["iw","ip","top","snd"] $
    t_soc prog_push $ replicate 10 idle

  putStrLn "prog_bus:"
  printProbe ["iw","ip","tx_bc","tx_wc","tx_in","tx_done","tx_out"] $
    t_soc prog_bus $ replicate 30 idle

  putStrLn "prog_loop:"
  printProbe ["iw","ip","top","snd","c"] $
    t_soc prog_loop $ replicate 30 idle

  putStrLn "prog_loop2:"
  printProbe ["iw","ip","top","snd","c"] $
    t_soc prog_loop2 $ replicate 30 idle

  putStrLn "prog_dbg:"
  printProbe ["iw","ip","top","snd","bus_dbg","bus_data"] $
    t_soc prog_dbg $ replicate 10 idle

  putStrLn "prog_call:"
  printProbe ["iw","ip","top","snd"] $
    t_soc prog_call $ replicate 20 idle

  putStrLn "prog_fun:"
  let (_, (dbg, out)) = e_soc_fun 1 2
  print dbg
  printProbe ["iw","ip","top","snd","bus_dbg","bus_data"] out


-- Full example, includes uploading program and starting CPU.

x_soc_boot = do
  let
    prog = Forth.compile p
    p = forever $ do
      -- just one bit transition. easy to check for
      push 0xff ; write uart_tx
      -- wait for tx ready. nop is needed for flag to clear.
      nop ; read uart_tx ; drop

    -- The 16-bit words are in big-endian form, and the most
    -- significant bit of the first word is the first serial bit sent.
    bytes = packProgram prog
    prog' = unpackProgram bytes
    prog_bits = map (toBitList 16)

    -- SPI transfer.  Don't oversample to keep example small.
    -- FIXME: add rle display to printProbe
    spi_c_d = spiBits Mode3 $ concat $ prog_bits prog'
    spi = [[1, 1, 0],
           [0, 1, 0]] ++
          [[0,sck,sda] | (sck,sda) <- spi_c_d] ++
          [[0, 1, 0],
           [1, 1, 0]]

    -- Clock it for a bit to make sure it works.
    postamble = replicate 100 [1,0,0]

  putStrLn "-- x_soc_boot"

  putStrLn "-- reference: execute from ROM"
  printProbe ["iw","ip","tx_bc","tx_wc","tx_in","tx_done","tx_out"] $
    t_soc prog $ replicate 30 $ t_soc_idle
  printL $ prog_bits prog'

  putStrLn "-- boot: execute from SPI booted RAM"
  printProbe ["run","iw","ip","tx_bc","tx_wc","tx_in","tx_done","tx_out"] $
    t_soc [] $ map ([1, 1] ++) $ spi ++ postamble ++ spi ++ postamble
  printL $ prog_bits prog'




t_mod_counter ins =
  snd $
  $(compile noProbe [] $
     \[] -> do (c,_) <- mod_counter 13 ; return [c])
  memZero ins
  
x_mod_counter = do
  putStrLn "x_mod_counter"
  print $ rle $ t_mod_counter $ replicate 30 []


-- -- Add-hoc way to test periodic signals
-- check_periodic :: [t] -> [t] -> Bool
-- check_periodic per@(_:_) seq@(_:_:_) = _ where
--   seq' = head $ take (length seq - 1) seq
  
  
-- check_periodic per seq =
--   error $ "check_periodic: invalid argument: " ++ show (len per, len seq)


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
  
  
  
