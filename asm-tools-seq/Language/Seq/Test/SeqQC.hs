-- QuickCheck is used to raise the abstraction level of tests.
--
-- The tests below all follow a 3-step construction process:
--
-- . Create a trace wrapper (t_) to transform the abstract (r S)
--   functions into the Int domain.
--
-- . From the trace wrapper, reate a single example (x_), that prints
--   out a human-readable report about how the function is performing.
--
-- . Once this works, generalize the example into property tests (p_)
--   to add test coverage, and comment out the verbose example.
--
--
-- To create traces there are essentially two mechanisms:
-- . allProbe gathers test probes (the <-- operator)
-- . noProbe gathers only the outputs of a dedicated Seq wrapper circuit
--
-- The allProbe approach is newer and has some formatting code for
-- printing logic traces.


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Language.Seq.Test.SeqQC where

import Language.Seq
import Language.Seq.Lib
import Language.Seq.CPU
import Language.Seq.Emu
import Language.Seq.Prim
import Language.Seq.Test.TestLib  -- for staging
import Language.Seq.Test.TestTools

import qualified Data.AsmTools.VCD as VCD

import qualified Language.Seq.Forth as Forth

import Language.Seq.TH(compile,compile',noProbe,allProbe)

import Prelude hiding (read, drop, catch)
import Data.Char
import Data.Bits
import Data.List hiding (drop)
import Data.List.Split
import Data.Bits
import Data.Maybe
import Data.Either
import Data.Array.IArray
import Test.QuickCheck hiding ((.&.),(.|.),again)
import Test.QuickCheck.Gen hiding (bitSize, getLine)
import Language.Haskell.TH
import System.Directory
import Control.Monad (when)

test :: IO ()
test = do

  -- print $ toWord [1,0,0,0]
  -- print $ toBitList 4 8
  -- print $ downSample' $ t_deser 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0,0,0,0,1]]

  -- x_async_receive_sample_emu
  -- x_async_receive_sample
  -- x_async_receive_emu
  x_th_async_receive
  x_async_receive
  x_mem
  x_st_mem
  x_fifo

  qc "p_si_edge" p_si_edge
  qc "p_bits" p_bits
  qc "p_sample" p_sample
  qc "p_deser" p_deser
  qc "p_async_receive" p_async_receive
  qc "p_spi" p_spi
  qc "p_rmii_rx" p_rmii_rx
  qc "p_fifo" p_fifo
  qc "p_channel" p_channel

  qc "p_soc_fun" p_soc_fun

  qc "p_async_transmit" p_async_transmit
  -- x_async_transmit

  x_stack
  x_spi
  x_rmii_rx                               
  x_mod_counter
  x_soc
  x_soc_boot
  x_deser

  x_st_testbench

  x_channel0

  -- x_sync_mod
  qc "p_sync_mod" p_sync_mod

-- Tests for library code.
--    t_  Trace wrapper (_emu or _th)
--    e_  Higher level test evaluator  fst=bool
--    x_  Example printout
--    p_  Property test

--    _emu SeqEmu code.  Slow, but flexible.
--    _th  TH code. Fast, but only pure target semantics.

-- deser



-- Note: this is an example case for a bit->bit signature, sugint the
-- specialized trace_b_b function.  This requires only two functions:
-- the parameterized test e_ and the property spec p_.

e_si_edge ins = f ins == f' ins where
  f           = trace_b_b si_edge
  f' i@(_:di) = 0 : (zipWith xor i di)

p_si_edge = forAll vars $ e_si_edge where
  vars = listOf1 $ word 1




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
  seqRunOuts $
  $(compile noProbe [1] $
    \[i] -> async_receive_sample 8 i >>= list2)
  memZero $ TestInput ins

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
  seqRunOuts $
  $(compile allProbe [1] $
    \[i] -> async_receive 8 i >>= list2)
  memZero $ TestInput ins

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
  seqRunOuts $
  $(compile noProbe [1,1,8] $
     \[bc,wc,tx] -> async_transmit bc (wc, tx) >>= list2)
  memZero $ TestInput ins

-- FIXME: Multiple bytes.  This is not easy to test due to done->next
-- word dependency.  Maybe make a CPU program?
e_async_transmit tx_byte = ([tx_byte] == rx_bytes, (tx_out, rx_bytes)) where
  tx_ins = take 100 $ [[0,0,0],[0,0,0],[1,1,tx_byte]] ++
           -- 8x oversampling
           (cycle $ replicate 7 [0,0,0] ++ [[1,0,0]])
               
  tx_out = t_async_transmit tx_ins
  -- use the receiver to test the transmitter
  rx_bytes = async_receive_bytes 8 $ map head tx_out

p_async_transmit = forAll vars pred where
  vars = word 8
  pred = fst . e_async_transmit

x_async_transmit = do
  let (_, (tx_out, rx_bytes)) = e_async_transmit 0x5A -- 90
  putStrLn "-- x_async_transmit"
  putStrLn "rx-bytes:"
  print $ rx_bytes
  putStrLn "tx:"
  printL $ tx_out
  


-- Using SeqTH it is not possible to generate outputs in response to
-- inputs at the test bench level.  This is annoying, but for now not
-- really an issue since it is possible to either go to SeqEmu, or
-- provide feedback at the Seq level.  It seems simplest to start
-- using CPU-like sequencers for that.  See below.


-- spi

t_spi Mode0 i = $(compile allProbe [1,1,1] $ d_spi Mode0 8) memZero $ TestInput i
t_spi Mode1 i = $(compile allProbe [1,1,1] $ d_spi Mode1 8) memZero $ TestInput i
t_spi Mode2 i = $(compile allProbe [1,1,1] $ d_spi Mode2 8) memZero $ TestInput i
t_spi Mode3 i = $(compile allProbe [1,1,1] $ d_spi Mode3 8) memZero $ TestInput i

e_spi (mode, bytes)  = (bytes == bytes', (bytes', table)) where
  (cpol, cpha) = spi_mode mode
  ins    = [[1,cpol,0]] ++
           [[0,c,d] | (c,d) <- upSample 2 $ spiBits mode bits] ++
           [[1,cpol,0]]

  bits   = concat $ map (toBitList 8) bytes
  bytes' = downSampleCD $ selectSignals ["s_wc","s_w"] probes outs
  table@(probes, (_, outs)) = t_spi mode ins

x_spi = do
  let cfg' = (Mode0, [202,123])
      cfg@(_,bytes)  = (Mode1, [4,5])
      (_, (bytes', (probes, (_, outs)))) = e_spi cfg

  putStrLn "-- x_spi"
  print bytes
  print bytes'
  printProbe ["s_wc","s_w","sclk","sdata","s_bc"] $ (probes, outs)

p_spi = forAll vars $ fst . e_spi where
  vars = do
    bytes <- listOf $ word 8
    mode  <- oneof $ map return [Mode0,Mode1,Mode2,Mode3]
    return (mode, bytes)


-- rmii

t_rmii_rx i = $(compile allProbe [1,1,1] $ d_rmii_rx 8) memZero $ TestInput i

e_rmii_rx bytes  = (bytes == bytes', (bytes', table)) where
  ins    = [[1,0,0]] ++
           [[0,rxd0,rxd1] | [rxd0,rxd1] <- bitPairs] ++
           [[1,0,0]]

  toBits = toBitList' LSBFirst 8

  bitPairs  = chunksOf 2 $ concat $ map toBits bytes
  bytes' = downSampleCD $ selectSignals ["rxwc","rxreg"] probes outs
  table@(probes, (_, outs)) = t_rmii_rx ins

x_rmii_rx = do
  let bytes = [0,1,2,3,4,5,6,7]
      (_, (bytes', (probes, (_, outs)))) = e_rmii_rx bytes

  putStrLn "-- x_rmii_rx"
  print bytes
  print bytes'
  printProbe ["crs_dv","rxd0","rxd1","rxreg","rxwc","rxaddr"] $ (probes, outs)
  saveVCD "v_rmii_rx.vcd" (probes, outs)

p_rmii_rx = forAll vars $ fst . e_rmii_rx where
  vars = listOf $ word 8


-- channel

-- d_channel* illustrate the use of the read/write channel rendez-vous
-- handshake pattern.  See comments in Lib.hs and TestLib.hs for more
-- information.  We feed the circuit with an external pulse to drive
-- the reader, and we probe the associated data stream (wc,c).

t_channel0 i = $(compile allProbe [1] d_channel0) memZero $ TestInput i

e_sync_rx t_channel (n_pulse,pulse_sep) = (ok, (expected, stream, table)) where
  ext_sync = rep n_pulse $ pulse pulse_sep pulse_sep
  ins = [[e] | e <- ext_sync]
  table@(probes, (_, outs)) = t_channel ins
  stream = downSampleCD $ selectSignals ["wc","w"] probes outs
  -- If pulse_sep gets small, some ext pulses will get missed because
  -- the writer is not ready to write yet, so here we just check that
  -- the sequence is correct, but not that there are enough elements.
  -- FIXME
  expected = take (length stream) $ cycle [4,5,6,7,12,13,14,15]
  ok = stream == expected

x_channel' probe_names t_channel spec = do
  let (ok, (expected, stream, (probes, (_, outs)))) = e_sync_rx t_channel spec
  putStrLn "-- x_channel0"
  print ok
  print expected
  print stream
  printProbe probe_names $ (probes, outs)
  
  saveVCD "v_channel0.vcd" (probes, outs)

-- x_channel0 = x_channel' ["ext","wc","w"] t_channel0 (16,4)
x_channel0 = x_channel' ["ext","wc","w"] t_channel0 (16,0)

p_channel = forAll spec $ fst . (e_sync_rx t_channel0) where
  spec = do
    n_pulse   <- choose (0,16)
    pulse_sep <- choose (0,10)
    return (n_pulse, pulse_sep)


-- cread_async_transmit

-- channel test for counter -> async_transmit
t_cread_async_transmit i = $(compile allProbe [1] d_cread_async_transmit) memZero $ TestInput i

e_cread_async_transmit n_wait = (ok, (channel_stream, rx_bytes, table)) where
  -- allow different bit clocks.
  bc_frame = [[1]] ++ rep n_wait [[0]]
  ins = rep 100 bc_frame
  table@(probes, (_, outs)) = t_cread_async_transmit ins
  tx_out   = map head $ selectSignals ["tx_out"] probes outs
  -- for inspecting the channel stream
  channel_stream = downSampleCD $ selectSignals ["tx_wc","tx_in"] probes outs
  -- verify uart output using Haskell ref UART from TestTools
  -- just discard bad frames (e.g. at the end of the trace)
  rx_bytes = rights $ uartRX (n_wait + 1) tx_out
  ok = head_equal [1..] rx_bytes

p_cread_async_transmit = forAll n_wait pred where
  n_wait = choose (0,8)
  pred = fst . e_cread_async_transmit

x_cread_async_transmit = do
  let (ok, (channel_stream, rx_bytes, (probes, (_, outs)))) = e_cread_async_transmit 0
  putStrLn "-- x_cread_async_transmit"
  print (ok, channel_stream)
  print rx_bytes
  printProbe ["tx_bc","tx_wc","tx_in","tx_rd_rdy", "tx_out"] $ (probes, outs)





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


x_st_mem = do
  putStrLn "-- x_st_mem"
  let [mem] =
        seqRunMems $
        $(compile allProbe [] $
           \_ -> closeMem [bits 16] $ \[rd] -> do
             -- Note that the address size needs to be set explicitly.
             -- Currently there is a bug in Seq Prim/TH code that
             -- needs to assume 64 bit signals when sizes are not
             -- defined.
             let wa = cbits 8 0
                 ra = wa
                 wd = rd
                 we = cbits 1 0
             return ([(we,wa,wd,ra)],[])) memZero $ TestInput $ replicate 10 []
  print $ elems mem
  
                                                   


-- fifo  (d_fifo is in TestSeqLib.hs to allow staging)

-- t_fifo_emu = trace [1,1,8] d_fifo
t_fifo ins = seqRunOuts $ $(compile noProbe [1,1,8] d_fifo) memZero $ TestInput ins

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

t_stack ins = seqRunOuts $ $(compile noProbe [1,1,8] d_stack) memZero $ TestInput ins

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


t_soc_idle = [1,1,1,0,0] -- [rx,tx_bc,cs,sck,sda]
t_soc_baud_div n = cycle ([[1,1,1,0,0]] ++ replicate (n-1) [1,0,1,0,0])

t_soc prog ins = (seqRunProbes res, seqRunOuts res) where
  res = $(compile allProbe [1,1,1,1,1] soc_test) [memRef prog] $ TestInput ins




-- Subroutine abstractions.  Note that this is not a 2-stack
-- machine, so the return address needs to be restored to the top
-- of stack before returning.
prog_fun v1 v2 = program $ do
  proc1 <- fun $ do push v1 ; swap ; ret
  proc2 <- fun $ do push v2 ; swap ; ret
  start $ forever $ do
    proc1 ; write dbg_addr
    proc2 ; write dbg_addr

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
      write uart_tx_addr
      nop                 -- tx_rd_rdy doesnt clear fast enough: FIXME: no longer needed?
      read  uart_tx_addr  -- waits until tx_rd_rdy is high
      drop                -- value returned is dummy
      jmp 0

    -- Uart "done" test, head-to-tail transmit
    -- This needs baud clock.  See t_soc call below.
    uart_tx =  c $ do
      push 0 ; write uart_tx_addr ; nop
      read  uart_tx_addr ; drop
      jmp 0

  
    -- Loop
    prog_loop = c $ do push 3; loop 1; jmp 0

    -- Same, using Forth control words
    prog_loop2 = c $ do begin; push 3; for; next; again

    -- Write to debug register
    prog_dbg = c $ do push 123; write dbg_addr

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
  printProbe ["iw","ip","tx_bc","tx_wc","tx_in","tx_rd_rdy","tx_out"] $
    t_soc prog_bus $ replicate 30 idle

  putStrLn "uart_tx:"
  printProbe ["iw","ip","tx_rd_rdy","tx_sr","tx_bc","tx_out"] $
    t_soc uart_tx $ take 200 $ t_soc_baud_div 8

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
      push 0xff ; write uart_tx_addr
      -- wait for tx ready. nop is needed for flag to clear.
      nop ; read uart_tx_addr ; drop

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
  printProbe ["iw","ip","tx_bc","tx_wc","tx_in","tx_rd_rdy","tx_out"] $
    t_soc prog $ replicate 30 $ t_soc_idle
  printL $ prog_bits prog'

  putStrLn "-- boot: execute from SPI booted RAM"
  printProbe ["run","iw","ip","tx_bc","tx_wc","tx_in","tx_rd_rdy","tx_out"] $
    t_soc [] $ map ([1, 1] ++) $ spi ++ postamble ++ spi ++ postamble
  printL $ prog_bits prog'




t_mod_counter ins =
  seqRunOuts $
  $(compile noProbe [] $
     \[] -> do (c,_) <- mod_counter 13 ; return [c])
  memZero $ TestInput ins
  
x_mod_counter = do
  putStrLn "x_mod_counter"
  print $ rle $ t_mod_counter $ replicate 30 []




-- Bit sync test.  It is not simple to define what "correct" means:
-- performance is data-dependent.  In practice this would need an
-- encoding scheme such as 8b/10b to guarantee transitions.  Once a
-- minimal number of transitions is guaranteed, a bound can be placed
-- on the frequency deviation.  TODO: Maybe add this on an as-needed
-- basis?

t_sync_mod period = trace [1,1] $ \[enable, idata] -> do
  sync <- sync_mod period enable idata
  return [sync, idata, enable]


e_sync_mod period pre post frac payload = (payload == payload', outs) where

  -- Signal consists of a couple of phases:
  -- . enable=0, data is ignored
  -- . enable=1, first edge starts recovery clock


  -- Idle level is the opposite of the first bit.  Usually payload
  -- contains a well-defined preamble.
  idle = 1 - head payload

  enable e = map (\i -> [e,i])

  ins =
    (enable 0 pre) ++
    -- Circuit detects the first edge when enable goes high, including
    -- the simultaneous edge, so provide a safe transition.
    (enable 0 [idle]) ++ 
    (enable 1 [idle]) ++ 
    (enable 1 $ fracSample frac payload) ++
    (enable 0 post)
  outs = t_sync_mod period ins
  payload' = map head $ downSample' outs
        

p_sync_mod = forAll vars prop where
  vars = do
    let mk = vectorOf 8 $ word 1
    pre <- mk
    post <- mk
    payload <- mk
    return (pre,post,payload)
  prop (pre,post,payload) =
    fst $ e_sync_mod 6 pre post (1.0 / 6.0) payload


x_sync_mod = do
  putStrLn "x_sync_mod"

  let pre  = [1,0,1,1,0,1,1]
      post = [1,0,0,1,1,1,0]
      frac = 1.0 / 5.9
      payload = [0,1,0,1,0,1,1,1]
      (_, signals) = e_sync_mod 6 pre post frac payload
      
  traverse print signals
  

x_st_testbench = do
  putStrLn "x_st_testbench"
  let tb o = return $ Nothing  -- Terminate after 1 step.
  print $ $(compile allProbe [1] $
            \[i] -> do
              i' <- inc i
              return [i'])
    memZero $ TestMachine [0] tb
    

-- Tests created using instantiated TH logic code.

async_receive_bytes div dataline = rx_bytes where
   rx_out = t_async_receive 8 $ map (:[]) dataline
   rx_bytes = map head $ downSample' rx_out
  


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


-- transposed trace wrappers for common cases, mapping state machines
-- to integer sequence functions.

-- FIXME: It would be great to be able to auto-generate these.

trace_b_b f is = map head $ f' $ transpose [is] where
  f' = trace [1] $ \[i] -> do o <- f i; return [o]

trace_bb_b f as bs = map head $ map f' $ transpose [as, bs] where
  f' = trace [1,1] $ \[a,b] -> do o <- f a b; return [o]

data ShowProbe = ShowInt Int | ShowIW Int
instance Show (ShowProbe) where
  show (ShowInt i) = show i
  show (ShowIW iw) = dasm iw

showProbe "iw" = ShowIW
showProbe _    = ShowInt

type Probe = (String,Int)
type Trace = ([Probe],[[Int]])

printProbe :: [String] -> Trace -> IO ()
printProbe columns (probes, signals) = do
  let signals' = selectSignals columns probes signals
      showcol = zipWith ($) (map showProbe columns)
  putStr $ showSignals' True columns $ map showcol signals'


saveVCD :: String -> Trace -> IO ()
saveVCD outFile' trace@(header,table) = do
  let vcd = VCD.toVCD "" (header, table)
      outFile = "/tmp/" ++ outFile'
  removeIfExists outFile
  writeFile outFile $ show $ vcd
  putStrLn $ "wrote " ++ outFile
  putStrLn ("- signals: " ++ show header)
  putStrLn ("- nb_samples: " ++ (show $ length table))

removeIfExists fileName = do
  exists <- doesFileExist fileName
  when exists $ removeFile fileName
  return ()
