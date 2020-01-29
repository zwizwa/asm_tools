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




-- Read/write synchronization pattern.
--
-- This is tricky.
--
-- To allow up to one transfer per clock, we need to use only one
-- delay in the sync path: RDY -> ACK -> RDY.  To do this, we express
-- the reading and writing end as combinatorial...
--
-- Writer: comb path from ACK -> RDY
-- Reader: comb path from RDY -> ACK
--
-- ... and glue the two together by breacking the circular path on the
-- ACK, meaing that the writer assumes the incoming ack refers to the
-- previous cycle.
--
-- In words: writer raises RDY, reader responds combinatorially to
-- produce an ACK whenever it has time to handle the RDY (i.e. there
-- might be cycles with no response), an writer sees the delayed ACK
-- in the next cycle where it can decide to produce a new value.
--

-- Illustrate with some examples.
-- ex0: Only do handshake.  Writer only writes once.
-- Writer has 3 states:
-- . wait for read to happen
-- . idle
-- The cross-wiring delays ack.
sync_ex0_write ack = do
  let rdy0 = 1  -- Start out with write ready
  closeReg [bits' 1 rdy0] $ \[rdy] -> do
    [rdy'] <- cond
      [(ack, [cbit 0])]
      [rdy]
    return ([rdy'],rdy')

sync_ex0_read ext rdy = do
  next <- ext `band` rdy
  [ack] <- cond
    [(next, [cbit 1])]
    [cbit 0]
  return ack

d_sync_ex0 [ext] =
  closeReg [bits 1] $ \[d_ack] -> do
    rdy  <- sync_ex0_write d_ack
    ack  <- sync_ex0_read ext rdy
    "ext"   <-- ext
    "rdy"   <-- rdy    -- combinatorial rdy
    "ack"   <-- ack    -- combinatorial ack
    "d_ack" <-- d_ack  -- delayed ack
    return ([ack],[])

-- Driving this circuit from a single pulse on ext gives the following
-- trace.  First line is writer rdy, waiting for read, second line is
-- ext signal causing a read to happen, producing combinatorial ack,
-- and third line is that combinatorial ack causing the writer to
-- transition, which in this case is idle (not rdy) state.  The ack
-- pulse is only 1 wide.
--
-- ext rdy ack d_ack
-- -----------------
--   0   1   0     0 (5x)
--   1   1   1     0
--   0   0   0     1
--   0   0   0     0 (19x)




-- ex1: also transfer some data
--
-- 1. writer is a counter that writes out the next state.  To make it
-- a bit more interesting, only write out values that have bit 2 set
-- and let the state machine continue without writing when bit is not
-- set.  E.g. 4 will be the first written output.
sync_ex1_write d_ack = do
  closeReg [bits 4] $ \[cnt] -> do

    cnti   <- cnt `add` 1
    wr_rdy <- slice' cnt 3 2
    
    [cnt'] <- cond
      [(d_ack,  [cnti]), -- ack always advances machine
       (wr_rdy, [cnt])]  -- when writing, wait for ack
      [cnti]             -- not writing, no ack: continue count
    return ([cnt'], (wr_rdy, cnt'))

-- 2. reader is synchronized to an external pulse.  if the writer is
-- ready when that pulse arrives, reader will send an ack.
sync_ex1_read ext rdy = do
  next  <- ext `band` rdy
  [ack] <- cond
    [(next, [cbit 1])]
    [cbit 0]  -- not ready or no ext pulse
  return ack


-- 3. the two machines need to be "cross-wired" by inserting a delay
-- on the ACK line.
d_sync_ex1 [ext] =
  closeReg [bits 1] $ \[d_ack] -> do
    (rdy, cnt) <- sync_ex1_write d_ack
    ack        <- sync_ex1_read ext rdy
    "ext"   <-- ext
    "rdy"   <-- rdy    -- combinatorial rdy
    "ack"   <-- ack    -- combinatorial ack
    "cnt"   <-- cnt    -- channel data (combinatorial, follows rdy)
    "d_ack" <-- d_ack  -- delayed ack
    return ([ack],[])






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



