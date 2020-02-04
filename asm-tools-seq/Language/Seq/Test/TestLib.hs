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
-- Writer: ACK -> RDY
-- Reader: RDY -> ACK
--
-- This introduces a cycle that needs to be broken when the two
-- machines are combined.  It seems most convenient to do that on the
-- ack, e.g. writer sees the ack generated in the previous cycle.
--
-- So the handshake looks like this:
--
-- 1. writer raises RDY.
--
-- 2. reader responds combinatorially to produce an ACK whenever it
--    has time to handle the RDY (i.e. there might be cycles with no
--    response).
--
-- 3. writer sees the delayed ACK in the next cycle where it can
--    decide to produce a new value combinatorially.
--
-- Illustrated below with some examples.
--
-- ex0: Only do handshake.  Writer only writes once.
-- Writer has 3 states:
-- . wait for read to happen
-- . idle
-- The cross-wiring is done using ack delay
sync_ex0_write ack = do
  let rdy0 = 1  -- Start out with write ready
  closeReg [bits' 1 rdy0] $ \[rdy] -> do
    [rdy'] <- cond
      [(ack, [cbit 0])]
      [rdy]
    return ([rdy'],rdy')

sync_ex_read ext rdy = do
  next  <- ext `band` rdy
  [ack] <- cond
    [(next, [cbit 1])]
    [cbit 0]
  return ack

d_sync_ex0 [ext] =
  closeReg [bits 1] $ \[d_ack] -> do
    rdy  <- sync_ex0_write d_ack
    ack  <- sync_ex_read ext rdy
    "ext"   <-- ext
    "rdy"   <-- rdy    -- combinatorial rdy
    "ack"   <-- ack    -- combinatorial ack
    "d_ack" <-- d_ack  -- delayed ack
    "cnt"   <-- cbit 0 -- dummy value
    return ([ack],[])

-- Controlling this circuit from a single pulse on ext gives the
-- following trace.  First line is writer rdy, waiting for ack, second
-- line is ext signal causing a read to happen, producing
-- combinatorial ack, and third line is the delayed ack causing the
-- writer to transition, which in this case is a finle idle (not rdy)
-- state.  The ack pulse is only 1 wide.
--
-- ext rdy ack d_ack
-- -----------------
--   0   1   0     0 (5x)
--   1   1   1     0
--   0   0   0     1
--   0   0   0     0 (19x)




-- ex1: Use synchronization to transfer a data stream
--
-- Writer is a counter that writes out the next state.  To make it a
-- bit more interesting, only write out values that have bit 2 set,
-- e.g. 4,5,6,7,12,14,14,15,...
--
sync_ex1_write d_ack = do
  closeReg [bits 1, bits 4] $ \[d_rdy,d_out] -> do

    -- Regarding the write there are essentially 3 cases:
    -- WAIT  d_rdy=1, d_ack=0  unacknowledged write  -> rdy=1, no stat change
    -- NEXT  d_rdy=1, d_ack=1  acknowledged write    -> rdy=? dep on state change
    -- IDLE  d_rdy=0, d_ack=x  no write, ignore ack
    --
    -- To complete the machine, one more piece of information is
    -- necesary: do we have data ready to send out.  It turns out to
    -- be simplest to just delay the output value, and use that to
    -- compute the possible next state of the counter, and then use
    -- that update to determine if we can write out the new value.
    
    cnt1 <- d_out `add` 1
    have <- slice' cnt1 3 2

    -- With this extra state update constraint we get the truth table.
    -- Some local values are used to make it easier to factor.
    --
    -- IN                 LOCAL   OUT
    -- d_rdy d_ack have   wait    rdy out     
    -- ------------------------------------
    -- 1     0     x      1       1   d_out
    -- 1     1     1      0       1   cnt1
    -- 1     1     0      0       0   cnt1
    -- 0     x     0      0       0   cnt1
    -- 0     x     1      0       1   cnt1

    n_d_ack <- inv d_ack
    wait    <- d_rdy `band` n_d_ack
    out     <- if' wait d_out cnt1
    rdy     <- wait `bor` have

    return ([rdy,out],(rdy,out))



-- The two machines need to be "cross-wired" by inserting a delay on
-- the ACK line.
d_sync_ex1 [ext] =
  closeReg [bits 1] $ \[d_ack] -> do
    (rdy, cnt) <- sync_ex1_write d_ack
    ack        <- sync_ex_read ext rdy
    "ext"   <-- ext
    "rdy"   <-- rdy    -- combinatorial rdy
    "ack"   <-- ack    -- combinatorial ack
    "cnt"   <-- cnt    -- channel data (combinatorial, follows rdy)
    "d_ack" <-- d_ack  -- delayed ack
    return ([ack],[])


-- Same as ex1, but with the state machine factored out behind a
-- generic ack/ready api.

sync_sm_ex cont =
  closeReg [bits 4] $ \[d_cnt] -> do
     cnt1 <- d_cnt `add` 1
     have <- slice' cnt1 3 2
     cnt' <- if' cont cnt1 d_cnt
     return ([cnt'], (have, cnt'))

d_sync_ex2 [ext] =
  closeReg [bits 1] $ \[d_ack] -> do
    (rdy, cnt) <- sync_sm_write sync_sm_ex d_ack
    ack        <- sync_ex_read ext rdy
    "ext"   <-- ext
    "rdy"   <-- rdy    -- combinatorial rdy
    "ack"   <-- ack    -- combinatorial ack
    "cnt"   <-- cnt    -- channel data (combinatorial, follows rdy)
    "d_ack" <-- d_ack  -- delayed ack
    return ([ack],[])


-- Attempt at a generic read/write channel interface.  This looks a
-- lot like a bus interface, so maybe should be treated as such?  See
-- TestLib.hs for the example this was lifted from.  Basic idea is
-- that we wrap this around a state machine sync_sm that takes a
-- 'cont' signal and produces a 'have' signal + data in response,
-- together with the next output of the machine.  FIXME: This might
-- need some time to sink in as a generic pattern.

-- FIXME: The circuit is actually symmetric: req and rdy are just
-- conventions that indicate the direction the data is flowing in.




-- FIXME: Create a test case for the closeChannel operator in Lib.hs
d_sync_ex3 [ext] = do
  let write d_rd_sync = do
        let wr_sync = cbit 1
            wr_data = cbit 1
        return (wr_sync, wr_data, [])
      reade _ wr_sync wr_data = do
        let rd_sync = cbit 1
        return (rd_sync, [])
  closeChannel reade write
  return []



-- UART DMA
--
-- In first attempt make it as simple as possible and do it ad-hoc.
-- The source state machine is just a counter.
--
-- FIXME: Ready and done are not the same.


-- sm_count d_wc = do
--   closeReg [bits 8] $ \[d_cnt] -> do
--     cnt1 <- d_cnt `add` 1
--     cnt <- if' d_wc cnt1 d_cnt
--     return ([cnt],d_cnt)


-- d_uart_dma_tx [bc] = do
--   closeReg [bits 1] $ \[d_wc] -> do
--     dat <- sm_count d_wc
--     (wc, ser_out) <- async_transmit_pull bc (d_wc, dat)
--     return ([wc],[])

d_uart_dma_tx [bc] = do
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



