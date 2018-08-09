-- A CPU?

-- It is the natural progression from Seq and Pru.
-- . implement CPU in Seq
-- . generalize assembler from Pru

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module CPU where
import Seq
import SeqLib
import Control.Monad
import Control.Applicative

-- How to begin?

-- First: architecture: a stack machine.

-- Memory seems to be the most important component.  I'm going to
-- target the iCE40, which has a bunch of individual memories,
-- allowing separate busses for instruction, data and return stack,
-- and the rest bundled as data memory.

-- At every clock, each of the 4 memories has a word sitting on its
-- read port:
-- i: current instruction
-- d: 2nd on stack  (top is in a register)
-- r: return address (ip is the instruction memory's write port)

-- The instruction word drives the decoder, which drives all the
-- muxes.

-- It seems that reading out instructions is the most useful thing to
-- start with.  This could be used for specialized sequencers that are
-- not necessarily general purpose CPUs.  This can then be gradually
-- extended to more abstract operations.


-- The main problem for building a CPU is to properly decompose the
-- decoder.  I'm not sure how to do this exactly, so just start in an
-- ad-hoc way.

-- There is some arbitraryness here: a hierarchy is created in the
-- nesting of the "close" operations.  The guideline is to abstract
-- away a register as soon as possible, i.e. move it to the inner part
-- of the hierarchy.

-- At the very top, there is:
-- . instruction memory access:
--   . read:  program sequencing
--   . write: bootloader
-- . BUS I/O (i.e. containing GPIO)

-- Each hierarchy level is an adaptation.  closeIW will abstract the
-- inner decoder as an iw -> jump operation, and insert the necessary
-- logic to either just advance to the next instruction, or perform a
-- jump.

data Ins r  = Ins {
  insRun  :: r S,
  insWord :: r S
  }

data Control r = Control {
  controlLoop :: r S,
  controlJmp  :: r S,
  controlArg  :: r S
  }

-- The interface to the outside consists of GPIO and iMem write access.
-- Maybe it is time to start parameterizing.

data IMemWrite r = IMemWrite {
  iMemWriteEn   :: r S,
  iMemWriteAddr :: r S,
  iMemWriteData :: r S
  }

-- Reads are closed, so sizes are necessarily taken from the size of
-- the write bus address and data registers.  If there is none
-- (e.g. if IMem is a ROM), then this can be used to specify size.
noIMemWrite :: Seq m r => Int -> Int -> IMemWrite r
noIMemWrite ibits abits = IMemWrite e w d where
  e = constant $ bit
  w = constant $ bits abits
  d = constant $ bits ibits


closeIMem :: Seq m r =>
  IMemWrite r
  -> r S
  -> (Ins r -> m (Control r, o))
  -> m o
closeIMem (IMemWrite wEn wAddr wData) run f = do
  t_wAddr <- stype wAddr
  t_wData <- stype wData
  closeMem [t_wData] $ \[iw] -> do
    (ipNext, o) <- closeReg [t_wAddr] $ \[ip] -> do
      (Control loop jump ipJump, o) <- f (Ins run iw)
      ipCont    <- inc ip
      [ipNext'] <- cond [(loop, [ip]), (jump, [ipJump])] [ipCont]
      -- run = synchronous reset, active low
      ipNext  <- if' run ipNext' 0
      return ([ipNext], (ipNext, o))  -- comb ip' to avoid extra delay
    return ([(wEn, wAddr, wData, ipNext)], o) 

-- A simple test for closeIMem:
-- . program outputs iw as output
-- . tied to a memory writer defined in the test lib

-- The next thing should be to test this on hardware, but this
-- requires code initialization primitives.


-- The origianl problem that drove this exploration is meanwhile
-- implemented on PRU.  These were the instructions needed:
-- 
-- a) loop n times
-- b) write UART byte, wait until done
-- c) wait
-- d) set I/O
-- e) read I/O into memory and advance pointer

-- To implement loops, it would be useful to have a stack to be able
-- to have nested loop counters.  This would mean less registers.  I'm
-- not going to be able to make this simpler than making a small forth
-- machine..  This way:

-- UART out can be bit-banged.
-- Multiple counters not needed for timing control.
-- No "wait" instruction needed: instruction counting suffices.
-- Add a data stack when needed.  Probably a single top register is enough.

-- The basic instructions seem straightforward.  This is just a
-- decoder that fans out into mux controls.  The unknown part to me is
-- the call/return.

-- Call:   move IP+1 -> rtop write port
--         inc rpointer
--         set ip from instruction word
-- Ret:    dec rpointer
--         move rtop -> IP

-- This could also be microcoded:
-- a) load literal into rdata
-- b) increment rstack
-- c) unconditional jump

-- The operations that can be reused are:
-- write, postinc  (stacks + buffers)
-- read, predec

-- So there is a clear tradeoff between the complexity of the
-- instruction decoder, and the amount of instructions needed.

-- Where to start?  Conditional memory write.

-- So for unidirectional flow, this is easy.  For bi-directional such
-- as a stack, two pointers need to be maintained.  It might be
-- simplest to initialize them such that the write/read operation can
-- happen immediately?  Both will have individual adders.  Maybe not a
-- good idea?




-- A generic bus.  Same structure as the memory interface.  Note that
-- we do not need to make this explicit in closeIMem.
data BUSIn r = BUSIn {
  busReadData :: r S
}
data BUSOut r = BUSOut {
  busWriteEn   :: r S,
  busWriteAddr :: r S,
  busWriteData :: r S,
  busReadAddr  :: r S
}




-- Memory reads take an extra cycle.  For instructions this is ok as
-- the instruction only needs to be available on the next cycle, but
-- for any other operation, the data will only be ready the next cycle.

-- Is that ok?

-- It seems so.


-- Perform an operation and wait for it to finish.
-- Let's keep the operation abstract, so what this does is:
--
-- . first time the instruction is executed, the sub-machine is
--   enabled.  the sequencer will wait until the machine provides a
--   "done" flag, which will advance the instruction pointer.
--
-- . it seems simpler to split this into "start" and "wait"
--   instructions.
--



