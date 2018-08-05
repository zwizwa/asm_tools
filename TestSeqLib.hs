-- see test-qc-SeqLib.hs
-- Separate module for SeqTH staging

{-# LANGUAGE FlexibleContexts #-}

module TestSeqLib where

import Seq
import SeqLib
import CPU
import Control.Monad


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


d_cpu_ins :: Seq m r => i -> m [r S]
d_cpu_ins _i = d_cpu $ \(Ins run iw) -> do
  ins  <- slice' iw 16 8
  arg8 <- slice' iw  8 0
  jmp  <- ins `equ` 0x80
  return (Jump jmp arg8, [iw, jmp, arg8])



-- Generic skeleton for CPU tests.
d_cpu :: Seq m r => (Ins r -> m (Jump r, [r S])) -> m [r S]
d_cpu decode = do
  -- Do not update the instruction pointer at the first instruction,
  -- as rData will be invalid.  The first cycle is used to perform the
  -- first instruction read.  After that, instruction pointer is updated.
  run <- seq01 -- 0,1,1,1....
  out <- closeIMem (noIMemWrite ibits abits) run decode
  return (run:out)
  where
    ibits = 16
    abits =  8

