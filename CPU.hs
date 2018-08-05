-- A CPU?

-- It is the natural progression from Seq and Pru.  Factor out some
-- Pru code to make a generic assembler, and implement the CPU in Seq.

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

-- How to begin?  Memory seems to be the most important component.
-- I'm going to target the iCE40, which has a bunch of individual
-- memories, allowing separate busses for instruction, data and return
-- stack, and the rest bundled as data memory.

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

-- data In s = In { i :: s } deriving (Functor, Foldable, Traversable)


cpu :: forall m r. (Num (r S), Seq m r) =>
  [(r S)] -> m ([(r S, r S, r S, r S)], [r S])
cpu ([i]) = do

  -- dispatch the instruction
  
  --c <- counter (SInt Nothing 0)
  --c' <- reg (SInt Nothing 0) $ \x -> add x 2
  let ip = i -- pointer chaser
  -- Instruction memory input registers.  Write is disabled.
  let iregs = (0, 0, 0, ip)
  return $ ([iregs],[i])



-- I have a particular problem in mind.  Some instructions needed
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





-- EDIT 20180805

-- Something simpler.  Make the simplest test case that allows the
-- generation of an output pattern based on a program.  The simplest I
-- can think of is a PWM pattern.  This is to test the JMP instruction.

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
  insWord :: r S
  }

data Jump r = Jump {
  jumpEn   :: r S,
  jumpAddr :: r S
  }

-- The interface to the outside consists of GPIO and iMem write access.
-- Maybe it is time to start parameterizing.

data IMemWrite r = IMemWrite {
  iMemWriteEn   :: r S,
  iMemWriteAddr :: r S,
  iMemWriteData :: r S
  }


closeIMem :: Seq m r =>
  IMemWrite r
  -> (Ins r -> i -> m (Jump r, o))
  -> i
  -> m o
closeIMem (IMemWrite wEn wAddr wData) f i = do
  t_wAddr <- stype wAddr
  t_wData <- stype wData
  closeMem [t_wData] $ \[iw] -> do
    (ip', o) <- closeReg [t_wAddr] $ \[ip] -> do
      (Jump jmp dst, o) <- f (Ins iw) i
      ipNext <- inc ip
      ip' <- if' jmp dst ipNext
      return ([ip'], (ip', o))  -- comb ip' to avoid extra delay
    return ([(wEn, wAddr, wData, ip')], o) 
             


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

  
