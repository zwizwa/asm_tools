{-# LANGUAGE ScopedTypeVariables #-}

module Language.Pru.BeagleLogic where
import Language.Pru
import Language.Pru.Weave

import Data.List

-- A weaver for the PRU1 data acquisition loop used in the
-- BeagleLogic.  It interleaves two loops:

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
