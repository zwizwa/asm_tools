-- see test-qc-SeqLib.hs
-- Separate module for SeqTH staging

module TestSeqLib where

import Seq
import SeqLib

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
