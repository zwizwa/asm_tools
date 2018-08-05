-- see test-qc-SeqLib.hs
-- Separate module for SeqTH staging

{-# LANGUAGE FlexibleContexts #-}

module TestSeqLib where

import Seq
import SeqLib
import CPU
import Control.Monad

fifo ta (rc,wc,wd) = do
  -- t: type
  -- d: data
  -- a: address
  -- c: clock enable
  td <- stype wd
  (wa,ra) <- closeReg [ta, ta] $ \[wa, ra] -> do
    wa1 <- inc wa
    ra1 <- inc ra
    ra' <- if' rc ra1 ra
    wa' <- if' wc wa1 wa
    return ([wa',ra'], (wa,ra))
  closeMem [td] $ \[rd] -> do
    return ([(wc, wa, wd, ra)], rd)


d_fifo i@[rc,wc,wd] = do
  let ta = SInt (Just 4) 0
  td <- stype wd
  rd <- fifo ta (rc,wc,wd)
  rc' <- delay rc  -- compensate for read delay
  return (rc':rd:i)

d_cpu_ins :: Seq m r => i -> m [r S]
d_cpu_ins _i = do
  let ibits = 16
      abits = 8
      -- Memory size is determined by write interface.
      -- FIXME: abstract as noMemWrite (bits)
      mw = noIMemWrite ibits abits
      decode (Ins run iw) = do
        -- FIXME: use run to enable all register updates
        ins  <- slice' iw 16 8
        arg8 <- slice' iw  8 0
        jmp  <- ins `equ` 0x80
        return (Jump jmp arg8, [iw, jmp, arg8])

  -- Do not update the instruction pointer at the first instruction,
  -- as rData will be invalid.  The first cycle is used to perform the
  -- first instruction read.  After that, instruction pointer is updated.
  run <- seq01 -- 0,1,1,1....
  out <- closeIMem mw run decode
  return (run:out)




