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
      decode (Ins iw) = do
        ins  <- slice' iw 16 8
        arg8 <- slice' iw  8 0
        jmp  <- ins `equ` 0x80
        return (Jump jmp arg8, [iw, jmp, arg8])

  -- rData is not valid on reset.  Wait one cycle.
  run <- delayRun
  out <- closeIMem mw run decode
  return (run:out)

-- Skip the first clock, since rData will be undefined until we clock
-- it out.  FIXME: why does this need two registers?
delayRun :: Seq m r => m (r S)
delayRun =
  delay <=<
  closeReg [bit' 0] $ \[r] -> return ([1], r)


