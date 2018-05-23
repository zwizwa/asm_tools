{-# LANGUAGE ScopedTypeVariables #-}

module BeagleLogic where
import Data.List
import Pru
import Weave

-- A weaver for the PRU1 data acquisition loop used in the
-- BeagleLogic.  It interleaves two loops:

-- a) Sample (possibly combined with other I/O control)
sample :: Pru m => [m ()]
sample = do
  r <- [21..28]
  b <- [0,1,2,3]
  return $ mov (Rb r b) (Rb 31 0)

-- b) data transfer to PRU0 + loop control
transfer :: forall m. Pru m => Int -> L -> [m ()]
transfer nb_samples again = fill ++ tail where
  fill =
    nops $ nb_samples - length tail
    :: [m ()]
  tail =
    [xout 10 (R 21) 32,   -- Move data across the broadside
     ldi  (R 31) 36,      -- Interrupt PRU0
     jmp again]
    :: [m ()]
              
-- These are woven together to produce the same skeleton as in the
-- original BeagleLogic code, but parameterized to be able to play
-- with it a bit.

-- The a) and b) loops need to be aligned such that XOUT in loop b)
-- and MOVs in loop a) align correctly.  Easy to see in the ASM
-- output.

bl_weave :: forall m. Pru m => [m()] -> m L
bl_weave loop1 = do
  loop_start <- declare
  let pre   = 2  -- this aligns xout with mov (see asm output)
      pad   = nops :: Int -> [m()]
      loop1 = sample :: [m()]
      loop2 = transfer (length loop1) loop_start :: [m()]

  entry <- label'
  shift_loops pre pad loop_start loop1 loop2
  return entry

