{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


-- Is it possible to capture enough of a synchronous state machine to
-- be able to do the same thing as with Pru.hs?  In general, a HDL
-- represents a discrete event simulator.  For clocked circuits, the
-- simulation becomes a lot simpler.

-- At every tick, register inputs are read, and an update function is
-- computed for each register.  So the basic unit to work with is the
-- register.

-- For now, assume MyHDL as a target.  The idea is to produce blocks
-- that look like:

--    @always_seq(CLK.posedge, reset=None)
--    def counter():
--        count.next = count + 1

-- Abstract the CLK and reset completely.

-- How to construct an embedded language around this idea?  There are
-- essentially two elements:
-- 1) combinatorial functions
-- 2) registers

-- A register is directly tied to the function that computes its next
-- state, so it makes sense to use a Map for this.

-- MyHDL doesn't use registers per se, but uses signals.  If a
-- signal's .next is written to, it behaves as a register.  In other
-- cases it is possible that a signal is just a wire.  I find this
-- very confusing.

import RTL
import RTLGen
import Data.Map.Strict (empty, foldrWithKey)



-- Let's start with a counter

counter :: forall m. RTL m => Signal -> m ()
counter b = do
  a  <- inv b
  b' <- add b $ L 1
  set b b'
  
main = do
  putStrLn " --- counter"
  printl $ mapToList $ compile $ signal >>= counter

printl es = sequence_ $ map print es
mapToList = foldrWithKey f [] where f k v t = (k,v):t
