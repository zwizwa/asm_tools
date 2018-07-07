{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}


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
import qualified SeqTerm
import qualified SeqExpr
import qualified SeqEmu
import qualified MyHDL
import qualified CPU
import qualified VCD
import qualified NetFun
import Data.Map.Lazy (empty, foldrWithKey, insert)
import qualified Data.Map.Lazy as Map
import qualified Control.Applicative as Applicative
import Control.Applicative (ZipList(..))
import Control.Category
import Prelude hiding((.),id)
import Control.Arrow
import Control.Monad
import Data.List
-- import Data.Functor.Apply


  
main = do

  putStrLn "--- test_mem_delay2"
  print $ test_mem_delay2

  
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

  putStrLn "--- counter SeqTerm.sexp'"
  printSeqTerm $ do c <- counter $ SInt Nothing 2 ; return [c]

  putStrLn "--- test_hdl"
  print_hdl test_hdl

  putStrLn "--- test_hdl_sync"
  print_hdl test_hdl_sync

  putStrLn "--- test_closeReg"
  print $ take 10 $ test_closeReg
  printSeqTerm $ closeReg2
  
  putStrLn "--- test_cpu_net"
  printSeqTerm $ test_cpu_net
  
  putStrLn "--- test_cpu_emu"
  print $ take 10 $ test_cpu_emu

  putStrLn "--- test_mem_delay"
  print $ take 10 $ test_mem_delay

  putStrLn "--- VCD"
  let vcd = VCD.toVCD "1ns" ([("d1",1),("d2",1),("d3",8)],
                             transpose [[0,1,1,0,0],[1,0,0,1,0],[1,2,3,3,3]])
  putStr $ show vcd
  writeFile "test.vcd" $ show vcd

  putStrLn "--- NetFun"
  NetFun.test

-- printSeqTerm :: Functor f => SeqTerm.M (f (SeqTerm.R S)) -> IO ()
printSeqTerm :: SeqTerm.M [SeqTerm.R S] -> IO ()
printSeqTerm src = do
  let (output, bindings) = SeqTerm.compile src
  putStrLn "-- bindings: "
  printl $ bindings
  putStrLn "-- output: "
  print output
  let inl = SeqExpr.inlined bindings
  putStrLn "-- inlined: "
  putStr $ SeqExpr.sexp' inl



printSeqEmu :: SeqEmu.M (SeqEmu.R S) -> IO ()
printSeqEmu src = do
  let src'    = src >> return ((),[]) -- API stub
      s0      = SeqEmu.reset src'
      f       = SeqEmu.tick src'
  putStrLn "init: "
  printl $ mapToList $ fst s0
  putStrLn "post: "
  printl $ mapToList $ fst $ fst $ f s0



printl es = sequence_ $ map print es
mapToList = foldrWithKey f [] where f k v t = (k,v):t



square = do
  c <- counter $ SInt (Just 3) 0
  bit =<< slr c 2

test_counter = SeqEmu.trace $ do
  c1 <- counter $ SInt (Just 1) 0
  c2 <- counter $ SInt (Just 3) 0
  -- [] is a meta-language construct needed for trace
  return [c1, c2]

closeReg2 = do
  let t = SInt Nothing 0
  closeReg [t, t] $ \[a, b] -> do
    a' <- add a 2
    b' <- add b 3
    return ([a', b'], [a, b])

test_closeReg = SeqEmu.trace closeReg2


-- For testing, outputs need to be collected in lists.
test_edge = SeqEmu.trace $ do
  e <- edge =<< square
  return [e]

-- Clock synchronizer
test_sync = SeqEmu.itrace f is where
  is = cycle [[v] | v <- [1,0,0,0,0,1,0,0]]
  f [i] = do
    o <- sync (SInt (Just 2) 0) i
    -- o <- counter (SInt (Just 2) 0)
    -- o <- edge i
    return [i,o]
  

-- Bare bones closeMem test.
dummy_mem ([_]) = do       -- memory's output
  let z = 0
  return ([(z, z, z, z)],  -- memory's input
          [])              -- test program empty output bus
test_mem :: [[Int]]
test_mem = SeqEmu.trace m  where
  t = SInt Nothing 0
  m = SeqEmu.closeMem ([t]) dummy_mem


-- After thinking a bit, I want this interface:

-- a) Do not put closeMem in the generic Seq.  Memories are an
--    external thing, so keep them as abstract as possible.  SeqEmu is
--    ok.
--
-- b) Allow to group memories in a functor (Traversable,Zip)
  
dummy_mem2 ([mo1, mo2]) = do
  ([mi1],_) <- dummy_mem $ [mo1]
  ([mi2],_) <- dummy_mem $ [mo2]
  return $ ([mi1, mi2],[])

test_mem2 = SeqEmu.trace m where
  t = SInt Nothing 0
  m = SeqEmu.closeMem ([t,t]) dummy_mem2

-- Input/output delay.
test_mem_delay = SeqEmu.trace m  where
  t = SInt Nothing 0
  m = SeqEmu.closeMem [t] $ \[rd] -> do
    c <- counter $ SInt (Just 3) 0
    return ([(1, 0, c, 0)], [c, rd])

-- wEn, wAddr, wData, rAddr
test_mem_delay2 = out where
  -- s0 = SeqEmu.reset m
  out = take 10 $ SeqEmu.trace m
  t = SInt Nothing 0
  m = SeqEmu.closeMem [t] $ \[rd] -> do
      c <- counter $ SInt (Just 3) 0
      return ([(1, 0, c, 0)], [c, rd])


    

-- Stub out what doesn't fit.  The idea is to find a way to gracefully
-- have these two interpretations produce something useful: MyHDL code
-- and an emulation test.
test_cpu_net = do
  ([(a,b,c,d)], o) <- CPU.cpu $ [0]
  return $ o ++ [a,b,c,d]
test_cpu_emu =  SeqEmu.trace m where
  t = SInt Nothing 0
  m = SeqEmu.closeMem ([t]) CPU.cpu
  mem = Map.fromList $ [(n,n+1) | n <- [0..2]]





-- MyHDL export needs some wrapping to specify module I/O structure.
-- SeqTerm has support for this.
test_hdl :: SeqTerm.M [SeqTerm.R S]
test_hdl = do
  -- Define types and signals
  let t    = SInt (Just 1) 0
      t_sr = SInt (Just 8) 0
  io@[i,o,sr_o] <- SeqTerm.io [t,t,t_sr]
  -- Instantiate circuits
  i1        <- delay i
  i2        <- delay i1
  (sr_o',_) <- shiftReg t_sr i
  -- Bind outputs
  connect o i2   
  connect sr_o sr_o'
  return io

test_hdl_sync :: SeqTerm.M [SeqTerm.R S]
test_hdl_sync = do
  io@[i,o] <- SeqTerm.io [SInt (Just 2) 0, SInt (Just 2) 0]
  o' <- sync (SInt (Just 2) 0) i
  connect o o'
  return io

print_hdl src = do
  let (ports, bindings) = SeqTerm.compile src
  putStrLn "-- ports: "
  print ports
  putStrLn "-- bindings: "
  printl $ bindings
  let inl = SeqExpr.inlined $ bindings
  putStr $ SeqExpr.sexp' inl
  putStrLn "-- MyHDL: "
  putStr $ show $ MyHDL.myhdl ports inl
  return ()
  

-- Just types.  Kleisli composition can be done without wrapping using
-- (>=>). I don't really see the point in using the Category or Arrow
-- abstraction.


-- Non-wrapped kleisli composition    
test_arrow0 :: Seq m r => r S -> m (r S)
test_arrow0 = integral >=> integral

-- Wrapped, with (.) from Control.Category
test_arrow1 :: Seq m r => r S -> m (r S)
test_arrow1 = runKleisli $ i . i where i = Kleisli integral

-- With some Arrow syntax
test_arrow2 :: forall m r. Seq m r => r S -> m (r S)
test_arrow2 = runKleisli a where
  integral' = Kleisli (integral :: r S -> m (r S))
  a = proc x -> do
    x' <- integral' . integral' -< x
    id -< x'

-- Can also be used to create "expressions".
test_arrow3 :: forall m r. Seq m r => r S -> m (r S)
test_arrow3 x = (add x <=< add x) x


