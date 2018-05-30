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

module CPU where
import Seq
import SeqLib
import Control.Monad

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


cpu :: forall m r. (Num (r S), Seq m r) => [r S] -> m ([(r S, r S, r S, r S)], [r S])
cpu [i] = do
  c <- counter (SInt Nothing 0)
  c' <- reg (SInt Nothing 0) $ \x -> add x 2
  let ip = i -- pointer chaser
  -- Instruction memory input registers.  Write is disabled.
  let iregs = (0,0,0,ip)
  return $ ([iregs],[i,c,c'])

