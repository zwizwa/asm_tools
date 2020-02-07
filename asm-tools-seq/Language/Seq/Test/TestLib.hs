-- Separate module with Seq.Lib wrappers to allow for Seq.TH staging

{-# LANGUAGE FlexibleContexts #-}

module Language.Seq.Test.TestLib where

import Language.Seq
import Language.Seq.Lib
import Language.Seq.CPU

import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Numeric


d_fifo i@[rc,wc,wd] = do
  let ta = bits 4
  td <- stype wd
  rd <- fifo ta (rc,wc,wd)
  rc' <- delay rc  -- compensate for read delay
  return (rc':rd:i)


d_stack i@[push,pop,wd] = do
  let ta = bits 4
  en <- push `bor` pop
  rd <- stack ta en push wd
  return (rd:i)


d_spi mode bits [cs,sclk,sdata] = do
  sync_receive mode bits cs sclk sdata
  return []

-- Note that for SPI it makes sense to sample the clock, for RMII the
-- clock is already at 50MHz and it makes sense to run all the logic
-- at the same rate, so no explicit clock signal.

-- This is Test.Lib which should first


d_rmii_rx bits [crs_dv, rxd0, rxd1] = do
  (rxwc, rxreg) <- rmii_receive crs_dv rxd1 rxd0

  -- 2K for addr

  rxaddr <- fifoPtr (SInt (Just 11) 0) rxwc

  -- test probes
  "crs_dv" <-- crs_dv
  "rxd0"   <-- rxd0
  "rxd1"   <-- rxd1
  "rxreg"  <-- rxreg
  "rxwc"   <-- rxwc
  "rxaddr" <-- rxaddr
  -- test bench uses named test rpobes
  return []



-- CHANNEL WRITERS
-- See also examples in Lib.hs

-- This one isn't useful as a general purpose machine, but is there to
-- illustrate a case where the writer isn't always ready.  See
-- SeqQC.p_channel property test.
cwrite_busycount d_rd_sync =
  closeReg [bits 4] $ \[d_cnt] -> do
     cnt1    <- d_cnt `add` 1
     wr_sync <- slice' cnt1 3 2
     cnt'    <- if' d_rd_sync cnt1 d_cnt
     return ([cnt'], (wr_sync, cnt', ()))

d_channel0 [ext] = do
  let read  = cread_sample ext
      write = cwrite_busycount
  (rd_out, _) <- closeChannel read write
  let (wc, w) = rd_out
  "wc"  <-- wc
  "w"   <-- w
  "ext" <-- ext
  return []




-- UART DMA
--
-- In first attempt make it as simple as possible and do it ad-hoc.
-- The source state machine is just a counter.
--
d_cread_async_transmit [bc] = do
  let write = cwrite_count 8
      read  = cread_async_transmit bc
  closeChannel read write
  return []








-- class ToList a b where
--   toList :: a -> [b]

-- instance Seq m r => ToList (r t, r t) [r t] where
--   toList (a,b) = return [a,b]
  



-- CPU tests.

-- This is not easy!

-- d_cpu_push :: Seq m r => i -> m [r S]
-- d_cpu_push _i = d_cpu $ \(Ins run iw) -> do
--   ins  <- slice' iw 16 8
--   arg8 <- slice' iw  8 0
--   closeMem [bits 16] $ \[snd] -> do
--     closeReg [bits 16, bits 8] $ \[top, saddr] -> do
--       -- working register push/pop
--       sup   <- saddr + 1
--       sdown <- saddr - 1
      
--       (top', sAddr) <-
--         switch [(1, (snd, sdown)] -- drop
--                [(2, (top, sup))]
  
             

  
  
--   jmp  <- ins `equ` 0x80
--   return (Jump 0 0, [])


-- Simple instruction decoder.
-- Define instructions next to the decoder.



