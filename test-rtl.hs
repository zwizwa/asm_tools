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
import qualified RTLNet as Net
import qualified RTLEmu as Emu
import Data.Map.Lazy (empty, foldrWithKey, insert)



-- Let's start with a counter


  
main = do
  putStrLn " --- counter Net"
  printNet counter

  putStrLn " --- counter Emu"
  printEmu counter

--  putStrLn " --- test_edge"
--  printEmu $ test_edge

printNet :: Net.M (Net.R S) -> IO ()
printNet = printl . mapToList . Net.compile

printEmu :: Emu.M (Emu.R S) -> IO ()
printEmu src = do
  printl $ mapToList $ Emu.compile src (insert 0 0 empty)
  print $ Emu.init src

printl es = sequence_ $ map print es
mapToList = foldrWithKey f [] where f k v t = (k,v):t



 

-- Examples

inc :: RTL m r => r S -> m (r S)
inc c = int 1 >>= add c

-- counter :: RTL m r => r S -> m ()
-- counter = regFix inc

-- regFix :: RTL m r => (r S -> m (r S)) -> r S -> m ()
-- regFix f r = f r >>= next r


delay d = do
  d0 <- signal -- create undriven signal
  next d0 d    -- set up register input
  return d0

edge d = do
  d0 <- delay d
  d `xor` d0


test_edge = square >>= edge

square = do
  n <- int 4
  c <- counter
  sll n c

-- counter :: forall m r. RTL m r => m (r S)
counter = do
  c <- signal
  c' <- inc c
  next c c'
  return c




-- Next: render m (r S) as [Int]




  
  
  
