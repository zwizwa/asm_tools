-- Separate module for Seq.TH staging

{-# LANGUAGE FlexibleContexts #-}

module Language.Seq.Test.Lib where

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


d_rmii bits [crs_dv, rxd0, rxd1] = do

  --let rxreg = undefined
  
  -- FIXME: logic!
  -- test probes
  "crs_dv" <-- crs_dv
  "rxd0"   <-- rxd0
  "rxd1"   <-- rxd1
  -- FIXME: check bit order and direction
  (rxwc, rxreg) <- rmii_receive crs_dv rxd1 rxd0
  "rxreg"  <-- rxreg
  "rxwc"   <-- rxwc
  
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



