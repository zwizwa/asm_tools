-- see test-qc-SeqLib.hs
-- Separate module for SeqTH staging

{-# LANGUAGE FlexibleContexts #-}

module TestSeqLib where

import Seq
import SeqLib
import CPU
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



