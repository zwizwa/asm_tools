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

import Seq
import SeqLib
import qualified SeqNet as Net
import qualified SeqEmu as Emu
import qualified MyHDL as MyHDL
import Data.Map.Lazy (empty, foldrWithKey, insert)



-- Let's start with a counter


  
main = do
  putStrLn "--- counter Emu.compile"
  printEmu $ counter (SInt (Just 2) 0)

  putStrLn "--- counter Emu.trace"
  print $ take 10 $ test_counter

  putStrLn "--- edge Emu.trace"
  print $ take 10 $ map head $ test_edge

  putStrLn "--- test_io"
  let (ports, signals) = Net.compile test_io
  print ports
  printl $ signals

  putStrLn "--- test_sync"
  printl $ take 10 $ test_sync

  putStrLn "--- test_io hdl"
  putStrLn $ MyHDL.gen $ Net.compile test_io

printEmu :: Emu.M (Emu.R S) -> IO ()
printEmu src = do
  let r0 = Emu.reset src
      f  = Emu.toTick' src
  putStrLn "init: "
  printl $ mapToList $ r0
  putStrLn "post: "
  printl $ mapToList $ f r0



printl es = sequence_ $ map print es
mapToList = foldrWithKey f [] where f k v t = (k,v):t



square = do
  n <- int 2
  c <- counter $ SInt (Just 3) 0
  slr c n >>= bit

test_counter = Emu.trace $ do
  c1 <- counter $ SInt (Just 1) 0
  c2 <- counter $ SInt (Just 3) 0
  -- [] is a meta-language construct needed for trace
  return [c1, c2]

-- For testing, outputs need to be collected in lists.
test_edge = Emu.trace $ do
  e <- square >>= edge
  return [e]

-- Clock synchronizer
test_sync = Emu.trace' f is where
  is = cycle [[v] | v <- [1,0,0,0,0,1,0,0]]
  f [i] = do
    o <- sync (SInt (Just 2) 0) i
    -- o <- counter (SInt (Just 2) 0)
    -- o <- edge i
    return [i,o]
  


-- MyHDL export needs some wrapping to specify module I/O structure.
-- Net has support for this.
test_io :: Net.M [Net.R S]
test_io = do
  io@[i,o] <- Net.io 2
  j <- delay i
  o' <- delay j  
  connect o o'   -- allow direct output
  return io




  
  
  
