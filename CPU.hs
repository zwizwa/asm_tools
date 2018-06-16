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

-- So memory was not easy to implement.  Got side tracked by a couple
-- of things.  Eventually, the implementation is quite simple though.
-- Next is to make a simple sequencer for a deterministic language
-- that can generate a sequence.

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





