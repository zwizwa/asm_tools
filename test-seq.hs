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
import qualified SeqNet as SeqNet
import qualified SeqEmu as SeqEmu
import qualified MyHDL as MyHDL
import Data.Map.Lazy (empty, foldrWithKey, insert)



-- Let's start with a counter


  
main = do
  putStrLn "--- counter SeqEmu.compile"
  printSeqEmu $ counter (SInt (Just 2) 0)

  putStrLn "--- counter SeqEmu.trace"
  print $ take 10 $ test_counter

  putStrLn "--- edge SeqEmu.trace"
  print $ take 10 $ map head $ test_edge

  putStrLn "--- test_sync"
  printl $ take 10 $ test_sync

  putStrLn "--- test_mem"
  print $ take 10 $ test_mem

  putStrLn "--- test_mem2"
  print $ take 10 $ test_mem2

--  putStrLn "--- test_mem3"
--  print $ take 10 $ test_mem3

  putStrLn "--- test_hdl"
  print_hdl test_hdl

  putStrLn "--- test_hdl_sync"
  print_hdl test_hdl_sync


print_hdl src = do
  let (ports, bindings) = SeqNet.compile src
  putStrLn "-- ports: "
  print ports
  putStrLn "-- bindings: "
  printl $ bindings
  printl $ SeqNet.inlined $ bindings
  putStrLn "-- MyHDL: "
  putStr $ MyHDL.gen (ports, bindings)


printSeqEmu :: SeqEmu.M (SeqEmu.R S) -> IO ()
printSeqEmu src = do
  let src' = src >> return ((),[]) -- API stub
      r0 = SeqEmu.reset src'
      f  = SeqEmu.toTick src'
  putStrLn "init: "
  printl $ mapToList $ r0
  putStrLn "post: "
  printl $ mapToList $ fst $ f r0



printl es = sequence_ $ map print es
mapToList = foldrWithKey f [] where f k v t = (k,v):t



square = do
  n <- int 2
  c <- counter $ SInt (Just 3) 0
  slr c n >>= bit

test_counter = SeqEmu.trace' $ do
  c1 <- counter $ SInt (Just 1) 0
  c2 <- counter $ SInt (Just 3) 0
  -- [] is a meta-language construct needed for trace
  return [c1, c2]

-- For testing, outputs need to be collected in lists.
test_edge = SeqEmu.trace' $ do
  e <- square >>= edge
  return [e]

-- Clock synchronizer
test_sync = SeqEmu.trace f is where
  is = cycle [[v] | v <- [1,0,0,0,0,1,0,0]]
  f [i] = do
    o <- sync (SInt (Just 2) 0) i
    -- o <- counter (SInt (Just 2) 0)
    -- o <- edge i
    return [i,o]
  

-- Bare bones memFix test.
dummy_mem rd = do     -- mem reg in
  z <- int 0
  return ((z, z, z),  -- mem regs out
          [z])        -- test program output
test_mem :: [[Int]]
test_mem = SeqEmu.traceIO empty m where
  t = SInt Nothing 0
  m = SeqEmu.fixMem (t,t) dummy_mem

-- Typically, you want to create multiple memories inside a program.
-- How to do that?  Let's start with just one.
dummy_mem2 memFix = do
  let t = SInt Nothing 0
  memFix (t, t) dummy_mem
test_mem2 :: [[Int]]
test_mem2 = SeqEmu.traceIO empty m where
  m = dummy_mem2 SeqEmu.fixMem

-- After thinking a bit, I want this interface:

-- a) Do not put the memFix inside the code.  Memories are an external
--    thing, so keep them as abstract as possible.
--
-- b) Below uses a list, but allow a generic functor
dummy_mem3 [rd1,rd2] = do
  m1 <- dummy_mem rd1
  m2 <- dummy_mem rd2
  return $ [m1, m2]

-- FIXME: memFix needs to be changed first to accomodate this.  

-- MyHDL export needs some wrapping to specify module I/O structure.
-- SeqNet has support for this.
test_hdl :: SeqNet.M [SeqNet.R S]
test_hdl = do
  io@[i,o] <- SeqNet.io 2
  j  <- delay i
  o' <- delay j  
  connect o o'   -- allow direct output
  return io

test_hdl_sync :: SeqNet.M [SeqNet.R S]
test_hdl_sync = do
  io@[i,o] <- SeqNet.io 2
  o' <- sync (SInt (Just 2) 0) i
  connect o o'
  return io
  

