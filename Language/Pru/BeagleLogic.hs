-- (c) 2018 Tom Schouten -- see LICENSE file

-- To understand this code, it might be helpful to first have a look
-- at the original BeagleLogic PRU1 assembly code.  It uses
-- predictable PRU instruction timing to create the time based used
-- for sampling.  The code here is a generalization of that idea,
-- using the same PRU0<->PRU1 interface.

-- Interleaved tasks
--
-- Essentially the original BeagleLogic PRU1 code contains 3 "tasks"
-- that are statically interleaved (time-domain multiplexed) inside a
-- loop body that iterates over 32 bytes = one block of buffer
-- registers, together with a pre-roll that starts up the loop.  The 3
-- tasks are:

-- 1. 32x per loop: NOP to create the time base
-- 2. 32x per loop: sample R31.b0 into one of 32 internal buffer registers
-- 3.  1x per loop: broadside transfer the 32 register bytes to PRU0

-- Weaving
--
-- In this code we use the same principle, but we make the instruction
-- interleaving programmable.  I.e. we use a macro processing step
-- that will take a description of the 3 loops and merge them
-- together.

-- Coroutines
--
-- One disadvantage of this approach is that the loop size is fixed,
-- transferring one packet per loop containing 32 samples.  This
-- imposes a strong constraint on the structure of the delay slot.
--
-- A way around this is to fill one slot with a coroutine call, and
-- use a coroutine on the other end that has its own control flow.
-- This uses up 2 instructions per cycle for the back and forth
-- coroutine calls, leaving intact the predictable timing.  As long as
-- the other coroutine takes constant time per call, the overall
-- timing remains rigid.

-- Review
--
-- The context of the problem that drove a solution like this is the
-- control of a slightly more complex data acquisition system.  While
-- the approach works, it is not simple to maintain proper timing by
-- static instruction scheduling.  The technique becomes feasible if
-- combined with constraint checking in emulation, especially because
-- the PRU code is hard to debug on the target.



{-# LANGUAGE ScopedTypeVariables #-}

module Language.Pru.BeagleLogic where
import Language.Pru
import Language.Pru.Weave

import Data.List

-- A weaver for the PRU1 data acquisition loop used in the
-- BeagleLogic.

-- It interleaves two loops:

-- a) Sample (possibly combined with other I/O control)

sample :: forall m. Pru m => [m ()]
sample = do
  r <- [21..28]
  b <- [0,1,2,3]
  return $ mov (Rb r b) (Rb 31 0)


-- b) data transfer to PRU0 + loop control
transfer :: forall m. Pru m => Int -> I -> [m ()]
transfer nb_samples again = fill ++ tail where
  fill =
    nops $ nb_samples - length tail
    :: [m ()]
  tail =
    [xout (I 10) (R 21) (I 32),
     ldi  (R 31) (I $ 20 + 16),      -- Interrupt PRU0
     jmp (Im again)]
    :: [m ()]
              
-- These are woven together to produce the same skeleton as in the
-- original BeagleLogic code, but parameterized to be able to play
-- with it a bit.

-- The a) and b) loops need to be aligned such that XOUT in loop b)
-- and MOVs in loop a) align correctly.  Easy to see in the ASM
-- output.

bl_weave :: forall m. Pru m => [m()] -> m I
bl_weave loop1 = do
  loop_start <- declare
  let pre   = 2  -- this aligns xout with mov (see asm output)
      pad   = nops :: Int -> [m()]
      loop2 = transfer (length loop1) loop_start :: [m()]

  entry <- label'
  shift_loops pre pad loop_start loop1 loop2
  return entry



-- Weave in a coroutine call.
sample_and_yield :: forall m. Pru m => Int -> Int -> [m ()]
sample_and_yield r1 r2 = thread where
  -- Create a yield thread
  n = length (sample :: [m ()])
  yields = replicate n yield
  yield = (jal (R r1) (Reg (R r2)))

  -- Merge the two :: [m ()] into a single :: [m ()]
  thread = zipWith (>>) yields sample
