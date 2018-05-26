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
import RTLLib
import qualified RTLNet as Net
import qualified RTLEmu as Emu
import Data.Map.Lazy (empty, foldrWithKey, insert)



-- Let's start with a counter


  
main = do
  putStrLn "--- counter Net.compile"
  printNet $ counter SInt'

  putStrLn "--- counter Emu.compile"
  printEmu $ counter SInt'

  putStrLn "--- counter Emu.trace"
  print $ take 10 $ Emu.trace test_counter

  putStrLn "--- edge Emu.trace"
  print $ take 10 $ Emu.trace test_edge

--  putStrLn " --- test_edge"
--  printEmu $ test_edge

printNet :: Net.M (Net.R S) -> IO ()
printNet = printl . mapToList . Net.compile

printEmu :: Emu.M (Emu.R S) -> IO ()
printEmu src = do
  let (r0, f) = Emu.compile src 
  putStrLn "init: "
  printl $ mapToList $ r0
  putStrLn "post: "
  printl $ mapToList $ f r0



printl es = sequence_ $ map print es
mapToList = foldrWithKey f [] where f k v t = (k,v):t



 

-- Examples



-- counter :: RTL m r => r S -> m ()
-- counter = regFix inc

-- regFix :: RTL m r => (r S -> m (r S)) -> r S -> m ()
-- regFix f r = f r >>= next r





square = do
  n <- int 2
  c <- counter SInt'
  slr c n >>= bit

test_counter = do
  c1 <- counter SInt'
  c2 <- counter SInt'
  -- [] is a meta-language construct needed for trace
  return [c1, c2]

-- For testing, outputs need to be collected in lists.
test_edge = do
  e <- square >>= edge
  return [e]



-- Next: render m (r S) as [Int]




  
  
  
