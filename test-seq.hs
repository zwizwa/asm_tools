{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}


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
import SeqSyntax
import Names
import SeqPrim
import TestTools
import qualified SeqArr
import qualified SeqApp
import qualified SeqStatic
import qualified SeqTerm
import qualified SeqExpr
import qualified SeqIfElse
import qualified SeqEmu
import qualified SeqTH
import qualified SeqC
import qualified MyHDL
import qualified CPU
import qualified VCD
import qualified CSV
import qualified NetFun

import Data.Map.Lazy (empty, foldrWithKey, insert, Map)
import qualified Data.Map.Lazy as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Applicative as Applicative
import Control.Applicative (ZipList(..))
import Control.Category
import Control.Arrow
import Control.Monad
import Prelude hiding (zipWith, (.), id)
import Data.List hiding (zipWith, zip)
import Data.Key(Zip(..),zipWith, zip)
import Data.Typeable
import Language.Haskell.TH as TH

import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

  
-- t_: trace
-- h_: hdl port module
-- x_: example
  
main = do

  x_counter
  x_counter2
  x_edge
  x_sync
  x_mem
  x_mem2
  x_mem_term
  x_counter_term
  x_hdl
  x_hdl_sync
  x_closeReg
  x_mem_write_to_read_delay
  x_vcd
  x_netfun
  x_template_haskell
  x_syntax
  x_ifs
  x_blink_fpga
  x_uart_fpga
  x_seqTH
  x_app_share
  x_st
  x_SeqC
  x_case
  x_SeqIfElse

x_counter = do
  putStrLn "--- x_counter"
  printSeqEmu $ counter (SInt (Just 2) 0)

x_counter_term = do
  putStrLn "--- x_counter_term"
  printSeqTerm $ do c <- counter $ SInt Nothing 2 ; return [c]


square = do
  c <- counter $ SInt (Just 3) 0
  b <- slr c 2
  band b 1

x_counter2 = do
  putStrLn "--- x_counter2"
  print $ take 10 $ t_counter2

t_counter2 = trace' $ do
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

x_closeReg = do
  putStrLn "--- x_closeReg"
  print $ take 10 $ t_closeReg
  printSeqTerm $ closeReg2

t_closeReg = trace' closeReg2


x_edge = do
  putStrLn "--- x_edge"
  print $ take 10 $ map head $ t_edge

t_edge = trace' $ do
  e <- edge =<< square
  return [e]

-- Clock synchronizer

x_sync = do
  putStrLn "--- tx_sync"
  printL $ take 10 $ t_sync
  
t_sync = trace [1] f is where
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
t_mem :: [[Int]]
t_mem = trace' m  where
  t = SInt Nothing 0
  m = closeMem ([t]) dummy_mem
x_mem = do
  putStrLn "--- x_mem"

x_mem_term = do
  putStrLn "--- x_mem_term"
  printSeqTerm $ closeMem ([bit]) dummy_mem


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
t_mem2 = trace' m where
  t = SInt Nothing 0
  m = closeMem ([t,t]) dummy_mem2
x_mem2 = do
  putStrLn "--- x_mem2"
  print $ take 10 $ t_mem2


-- wEn, wAddr, wData, rAddr
t_mem_write_to_read_delay = trace' m  where
  t = SInt Nothing 0
  m = closeMem [t] $ \[rd] -> do
    c <- counter $ SInt (Just 3) 0
    return ([(1, 0, c, 0)], [c, rd])

x_mem_write_to_read_delay = do
  putStrLn "--- x_mem_write_to_read_delay"
  print $ take 10 $ t_mem_write_to_read_delay

    



-- MyHDL export needs some wrapping to specify module I/O structure.
-- SeqTerm has support for this.
x_hdl = do
  putStrLn "--- x_hdl"
  print_hdl h_hdl
  
h_hdl :: SeqTerm.M [SeqTerm.R S]
h_hdl = do
  -- Define types and signals
  let t    = SInt (Just 1) 0
      t_sr = SInt (Just 8) 0
  io@[i,o,sr_o] <- SeqTerm.inputs [t,t,t_sr]
  -- Instantiate circuits
  i1    <- delay i
  i2    <- delay i1
  sr_o' <- shiftReg ShiftLeft t_sr i
  -- Bind outputs
  connect o i2   
  connect sr_o sr_o'
  return io

x_hdl_sync = do
  putStrLn "--- x_hdl_sync"
  print_hdl h_sync
  
h_sync :: SeqTerm.M [SeqTerm.R S]
h_sync = do
  io@[i,o] <- SeqTerm.inputs [SInt (Just 1) 0, SInt (Just 1) 0]
  o' <- sync (SInt (Just 1) 0) i
  connect o o'
  return io





x_template_haskell = do
  putStrLn "--- x_template_haskell"
  let (n,f) = $(named [| \[a,b,c] -> a |])
  print $ (n, f [1,2,3])

x_syntax = do
  putStrLn "--- x_syntax"
  let stx = $(seqFile "example.seq")
  print stx

x_seqTH = m1 >> m2 where
  m1 = do
    -- Print syntax
    putStrLn "-- x_seqTH (syntax)"
    let c@(outputs, bindings, probes) = SeqTerm.compileTerm' $ do
          en <- SeqTerm.input SeqLib.bit
          SeqTH.test [en]
    print outputs
    sequence $ map print bindings
    putStrLn $ pprint $ SeqTH.toExp c

  m2 = do
    -- Some ad-hoc tests for SeqTH,SeqPrim combo.
    let test f = print $ snd $ f mz $ map (:[]) [0..9]
        mz = cycle [const 0]
        -- FIXME: test is rank-2
    test $(SeqTH.compile [] [1] $ \[i] -> do c <- counter $ bits 3 ; return [c])
    test $(SeqTH.compile [] [4] $ \[i] -> do c <- integral i ; return [c])
    test $(SeqTH.compile [] [4] $ \[i] -> do c <- conc i (constant $ bits 1) ; return [c])
    test $(SeqTH.compile [] [4] $ \[i] -> do c <- slice i (Just 4) 1 ; return [c])
    test $(SeqTH.compile [] [1] $ \[i] -> do c <- if' i (constant $ bits' 2 3) (constant $ bits' 2 2) ; return [c])
    test $(SeqTH.compile [] [4] $ \[i] -> do c <- i `equ` 3; return [i,c])
    test $(SeqTH.compile [] [4] $ \[i] -> do c <- i `band` 5; return [i,c])
    test $(SeqTH.compile [] [4] $ \[i] -> do c <- i `sub` 2; return [i,c])

x_vcd = do
  putStrLn "--- x_vcd"
  let vcd = VCD.toVCD "1ns" ([("d1",1),("d2",1),("d3",8)],
                             transpose [[0,1,1,0,0],[1,0,0,1,0],[1,2,3,3,3]])
  putStr $ show vcd
  writeFile "test.vcd" $ show vcd

x_netfun = do
  putStrLn "--- x_netfun"
  NetFun.test

-- sequenced if' : does it need to be bundled?
x_ifs = do
  putStrLn "--- x_ifs"
  print_hdl $ do
    io@[c,i1,i2,o1,o2] <- SeqTerm.inputs $ replicate 5 bit
    os' <- ifs c [i1,i2] [0,0]
    sequence $ zipWith connect [o1,o2] os'
    return io

-- Applicative interface where all operations are expressed as pure
-- functions.  The problem here seems to be sharing.  But that can be
-- resolved like this:

x_app_share = do
  putStrLn "--- x_app_share"
  let c@(outputs, bindings) = SeqTerm.compileTerm m
      m = do a <- SeqApp.square $ inc 1
             return [a]
  print outputs
  sequence $ map print bindings

-- Blink-a-LED example for HX8K breakout
-- 12MHz
f_blink_fpga :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_blink_fpga =
  $(named
   [|
    \[ _LED0 ] -> do
      c <- counter $ (bits 24)
      led <- slice c (Just 23) 23
      connect _LED0 led
    |])
x_blink_fpga = do
  putStrLn "-- x_blink_fpga"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
      (py,pcf) = MyHDL.fpga' "x_blink_fpga" f_blink_fpga pin
  writeFile "x_blink_fpga.py" $ show py
  writeFile "x_blink_fpga.pcf" $ show pcf


-- UART

f_uart_fpga :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_uart_fpga =
  $(named
   [|
    \[ _RX,
       _LED0, _LED1, _LED2, _LED3,
       _LED4, _LED5, _LED6, _LED7
     ] -> do
      (wStrobe, wData) <- async_receive 8 _RX
      -- b0 <- slice' wData 1 0
      -- connect _LED0 b0
      let leds = [_LED0, _LED1, _LED2, _LED3,
                  _LED4, _LED5, _LED6, _LED7]
          connData n pin =
            slice' wData (n+1) n >>= connect pin
      sequence_ $ zipWith connData [0..] leds
    |])

x_uart_fpga = do
  putStrLn "-- x_uart_fpga"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
      (py,pcf) = MyHDL.fpga' "x_uart_fpga" f_uart_fpga pin
  writeFile "x_uart_fpga.py" $ show py
  writeFile "x_uart_fpga.pcf" $ show pcf


-- TOOLS

-- printSeqTerm :: Functor f => SeqTerm.M (f (SeqTerm.R S)) -> IO ()
printSeqTerm :: SeqTerm.M [SeqTerm.R S] -> IO ()
printSeqTerm src = do
  let (output, bindings) = SeqTerm.compileTerm src
  putStrLn "-- bindings: "
  printL $ bindings
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
  printL $ mapToList $ fst s0
  putStrLn "post: "
  printL $ mapToList $ fst $ fst $ f s0



mapToList = foldrWithKey f [] where f k v t = (k,v):t



trace ::
  [Int]
  -> ([SeqEmu.R S] -> SeqEmu.M [SeqEmu.R S])
  -> [[Int]] -> [[Int]]
trace typs fm is = os where
  os = SeqEmu.iticks fm' is
  fm' = SeqEmu.onInts typs fm

-- Specialized: no inputs.
trace' m = trace [] (\[] -> m) $ cycle [[]]


print_hdl :: SeqTerm.M [SeqTerm.R S] -> IO ()
print_hdl src = do
  let (ports, bindings) = SeqTerm.compileTerm src
  putStrLn "-- ports: "
  print ports
  putStrLn "-- bindings: "
  printL $ bindings
  let inl = SeqExpr.inlined $ bindings
  putStr $ SeqExpr.sexp' inl
  putStrLn "-- MyHDL: "
  putStr $ show $ MyHDL.myhdl "module" ports inl
  return ()
  




-- Examples.  See SeqKleisli.hs



-- Non-wrapped Kleisli Arrow
type SeqA m r a b = r a -> m (r b)

-- Non-wrapped kleisli composition    
x_arrow0 :: Seq m r => SeqA m r S S
x_arrow0 = integral >=> integral

-- Wrapped, with (.) from Control.Category
x_arrow1 :: Seq m r => SeqA m r S S
x_arrow1 = runKleisli $ i . i where i = Kleisli integral

-- With some Arrow syntax
x_arrow2 :: Seq m r => SeqA m r S S
x_arrow2 = runKleisli a where
  integral' = Kleisli integral
  a = proc x -> do
    x' <- integral' . integral' -< x
    id -< x'

-- Can also be used to create "expressions".
x_arrow3 :: Seq m r => SeqA m r S S
x_arrow3 x = (add x <=< add x) x




-- ST Monad doodling for SeqTH
-- The output should be a state transformer, e.g.

type A = ()

x_st = do
  print "-- x_st"
  print $ runST $ f_st

  

f_st :: ST s Int
f_st = do
  let addr = 3
      val  = 4
      size = 256
  a <- newArray (0, size) 0 :: ST s (STUArray s Int Int)
  writeArray a addr val
  v <- readArray a addr
  return v



x_SeqC = do
  let ct = SeqTerm.compileFun [SInt Nothing 0] f
      f [i] = do
        o <- i `mul` i
        return [o]

  print "-- x_SeqC"
  print $ SeqC.C ("fun", ct)



x_case = m where
  t = do
    i <- 1
    cond [(i,[1,2,3])] [4,5,6]
  
         
  c@(outputs, bindings) = SeqTerm.compileTerm t
  m = do
    putStrLn "-- x_case"
    printL bindings
    print outputs
    
  

x_SeqIfElse = do
  putStrLn "-- x_SeqIfElse"
  SeqIfElse.x $ do
    i <- SeqTerm.input (SInt (Just 1) 0)
    SeqLib.d_async_receive 8 i
  
