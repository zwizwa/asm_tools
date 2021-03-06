-- Ad-hoc "example tests" for Seq code.
-- TODO: clean up and move to seq-qc.hs

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonadFailDesugaring #-}

import Language.Seq
import Language.Seq.Lib
import Language.Seq.Syntax
import Language.Seq.Names
import Language.Seq.Prim
import Language.Seq.Test.Tools
import qualified Language.Seq.Lib as SeqLib
import qualified Language.Seq.Forth as Forth
import qualified Language.Seq.Arr as SeqArr
import qualified Language.Seq.App as SeqApp
import qualified Language.Seq.Static as Static
import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Expr as SeqExpr
import qualified Language.Seq.IfElse as SeqIfElse
import qualified Language.Seq.Emu as SeqEmu
import qualified Language.Seq.TH as SeqTH
import qualified Language.Seq.C as SeqC
import qualified Language.Seq.CPU as CPU
import qualified Language.Seq.NetList as SeqNetList

import qualified Data.AsmTools.MT as MT
import qualified Data.AsmTools.VCD as VCD
import qualified Data.AsmTools.CSV as CSV
import qualified Data.AsmTools.DigiView as DigiView
import qualified Data.AsmTools.NetFun as NetFun

import qualified Language.Seq.MyHDL as MyHDL
import qualified Language.Seq.MyHDLRun as MyHDLRun
import qualified Language.Seq.Verilog as Verilog
import qualified Language.Seq.VerilogRun as VerilogRun

import Data.Map.Lazy (empty, foldrWithKey, insert, Map, assocs)
import qualified Data.Map.Lazy as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Applicative as Applicative
import qualified Data.Set as Set
import Control.Applicative (ZipList(..))
import Control.Category
import Control.Arrow
import Control.Monad
import Prelude hiding (zipWith, (.), id, zip)
import Data.List hiding (zipWith, zip)
import Data.Maybe
import Data.Key(Zip(..),zipWith, zip)
import Data.Typeable
import Language.Haskell.TH as TH
import Control.Monad.ST
import Data.Array.Unboxed hiding (assocs)
import Data.Array.ST

import System.Environment
  
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
  x_closeReg
  x_mem_write_to_read_delay
  x_vcd
  x_netfun
  x_template_haskell
  x_syntax
  x_seqTH
  x_app_share
  x_st
  x_SeqC
  x_case
  x_SeqIfElse
  -- hdl
  x_ifs
  x_hdl
  x_hdl_sync
  x_run_myhdl
  x_run_verilog
  x_testbench
  x_verilog
  x_seqnetlist
  x_mt
  x_digiview

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

    



x_template_haskell = do
  putStrLn "--- x_template_haskell"
  let (n,f) = $(named [| \[a,b,c] -> a |])
  print $ (n, f [1,2,3])

x_syntax = do
  putStrLn "--- x_syntax"
  putStrLn "<disabled>"
  -- let stx = $(seqFile "example.seq")
  -- print stx

x_seqTH = m1 >> m2 where
  m1 = do
    -- Print syntax
    putStrLn "-- x_seqTH (syntax)"
    let c@(outputs, bindings, probes) = SeqTerm.compileTerm $ do
          en <- SeqTerm.input SeqLib.bit
          SeqTH.test [en]
    print outputs
    sequence $ map print bindings
    putStrLn $ pprint $ SeqTH.toExp c

  m2 = do
    -- Some ad-hoc tests for SeqTH,SeqPrim combo.
    let test f = print $ snd $ f mz $ TestInput $ map (:[]) [0..9]
        mz = cycle [const 0]
        -- FIXME: test is rank-2
    test $(SeqTH.compile (const False) [1] $ \[i] -> do c <- counter $ bits 3 ; return [c])
    test $(SeqTH.compile (const False) [4] $ \[i] -> do c <- integral i ; return [c])
    test $(SeqTH.compile (const False) [4] $ \[i] -> do c <- conc i (constant $ bits 1) ; return [c])
    test $(SeqTH.compile (const False) [4] $ \[i] -> do c <- slice i (Just 4) 1 ; return [c])
    test $(SeqTH.compile (const False) [1] $ \[i] -> do c <- if' i (constant $ bits' 2 3) (constant $ bits' 2 2) ; return [c])
    test $(SeqTH.compile (const False) [4] $ \[i] -> do c <- i `equ` 3; return [i,c])
    test $(SeqTH.compile (const False) [4] $ \[i] -> do c <- i `band` 5; return [i,c])
    test $(SeqTH.compile (const False) [4] $ \[i] -> do c <- i `sub` 2; return [i,c])

x_vcd = do
  putStrLn "--- x_vcd"
  let vcd = VCD.toVCD "1ns" ([("d1",1),("d2",1),("d3",8)],
                             transpose [[0,1,1,0,0],[1,0,0,1,0],[1,2,3,3,3]])
  putStr $ show vcd
  writeFile "test.vcd" $ show vcd

x_netfun = do
  putStrLn "--- x_netfun"
  NetFun.test


-- Applicative interface where all operations are expressed as pure
-- functions.  The problem here seems to be sharing.  But that can be
-- resolved like this:

x_app_share = do
  putStrLn "--- x_app_share"
  let c@(outputs, bindings, _) = SeqTerm.compileTerm m
      m = do a <- SeqApp.square $ inc 1
             return [a]
  print outputs
  sequence $ map print bindings


-- TOOLS

-- printSeqTerm :: Functor f => SeqTerm.M (f (SeqTerm.R S)) -> IO ()
printSeqTerm :: SeqTerm.M [SeqTerm.R S] -> IO ()
printSeqTerm src = do
  let (output, bindings, _) = SeqTerm.compileTerm src
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
  
         
  c@(outputs, bindings, _) = SeqTerm.compileTerm t
  m = do
    putStrLn "-- x_case"
    printL bindings
    print outputs
    
  

x_SeqIfElse = do
  putStrLn "-- x_SeqIfElse"
  SeqIfElse.x $ do
    i <- SeqTerm.input (SInt (Just 1) 0)
    (wc,w) <- SeqLib.async_receive 8 i
    return [wc,w]
  


    
-- sequenced if' : does it need to be bundled?
x_ifs = do
  putStrLn "--- x_ifs"
  print_hdl $ do
    io@[c,i1,i2,o1,o2] <- SeqTerm.inputs $ replicate 5 bit
    os' <- ifs c [i1,i2] [0,0]
    sequence $ zipWith connect [o1,o2] os'
    return io



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



-- Blink-a-LED example for HX8K breakout
-- 12MHz
f_blink_fpga :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_blink_fpga =
  $(named
   [|
    \[ _LED0 ] -> do
      c <- counter $ (bits 24)
      led <- slice' c 23 22
      connect _LED0 led
    |])
x_blink_fpga = do
  putStrLn "-- x_blink_fpga"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
      (py,pcf) = MyHDL.fpgaGen "x_blink_fpga" f_blink_fpga pin
  writeFile "x_blink_fpga.py" $ show py
  writeFile "x_blink_fpga.pcf" $ show pcf





-- Old test routine.  This is the only one that needs access to
-- MyHDL.myhdl directly.  In practice a little more metadata is needed
-- to instantiate the module.  See MyHDL.pyModule
print_hdl :: SeqTerm.M [SeqTerm.R S] -> IO ()
print_hdl src = do
  let (ports, bindings, _) = SeqTerm.compileTerm src
  putStrLn "-- ports: "
  print ports
  putStrLn "-- bindings: "
  printL $ bindings
  let inl = SeqExpr.inlined $ bindings
      portNames = ["p" ++ show n | (_,n) <- zip ports [0..]]
  putStr $ SeqExpr.sexp' inl
  putStrLn "-- MyHDL: "
  putStr $ MyHDL.myhdl "module" ports inl  -- low level interface
  return ()
  

x_run_myhdl = do
  putStrLn "-- x_run_myhdl"
  -- MyHDLRun.run_process "x_soc_fpga"
  MyHDLRun.test

x_testbench = do
  putStrLn "-- x_testbench"
  let mod [s, d] = do return $ [s, d]
      tb = MyHDL.testbench "x_testbench" [1,8] mod [[1,0],[0,0]]
  print tb


verilog_ops [i, o] = do
  n <- closeMem [bits 16] $ \[rd] -> do
    ra <- counter $ bits 8
    a <- inv i >>= delay
    b <- i `add` a
    c <- i `sub` a
    d <- i `mul` a
    e <- i `bxor` a
    f <- i `bor` a
    g <- i `band` a
    h <- i `sll` a
    x <- i `slr` a
    j <- i `equ` a
    k <- if' i a b
    l <- reduce' conc [a,b,c,d,e,f,g,h,x,j,k,rd,cbits 3 7]
    m <- slice' l 4 2
    n <- conc l m
    -- n should depend on all
    return ([(cbit 0, cbits 8 0, cbits 16 0, ra)], n)
  connect o n

x_verilog_ops = do
  putStrLn "-- x_verilog_ops"
  let mod = verilog_ops
      v = Verilog.vModule "mymod" ["IN", "OUT"] [bit, bit] mod
  print $ v
  writeFile "x_verilog_ops.v" $ show v

x_verilog = do
  x_verilog_ops

-- verilog_tb [] = do
--   closeMem [bits 16] $ \[rd] -> do
--     let we = cbits 1 1
--         wa = cbits 8 0
--         ra = cbits 8 0
--     wd <- add rd 0
--     return ([(we, wa, wd, ra)], [rd, wd])

x_run_verilog = do
  putStrLn "-- x_run_verilog"
  putStrLn "disabled: doesn't run well as part of main test suite.. see t_cosim"
  -- let mod [i] = do o <- add i 1 ; return [o]
  -- setEnv "SEQ_COSIM" "vpi/cosim"
  -- outs <- VerilogRun.trace [8] mod $ map (:[]) [0..10]
  -- print outs




  
x_seqnetlist = do
  putStrLn "-- x_seqnetlist"
  let mod = CPU.soc
      pbp@(ports, bindings, probes) = SeqTerm.compileFun (replicate 5 bit) CPU.soc
      SeqNetList.NetList ports' bindings' _ = SeqNetList.convert pbp
      dg = SeqNetList.toDG bindings'
      sorted = SeqNetList.sorted dg
      inlined = SeqNetList.inlined dg
      nodes = sort $ Set.toList $ Map.keysSet bindings'
  print ports'
  --printL $ assocs'
  putStrLn "--- sorted"
  printL $ [ (n, te, SeqNetList.deps dg n) | (n,te) <- sorted ]

  putStrLn "--- inlined"
  printL $ inlined

  -- 
  --putStrLn "--- fanout"
  --printL $ [ (n, SeqNetList.allFanout dag n) | n <- nodes ]

  putStrLn "--- fanout"
  printL $ [ (n, bindings' Map.! n, SeqNetList.fanout dg n) | n <- nodes ]
  

  -- Print out the individual dependencies
  -- printL $ catMaybes $ do
  --   a <- nodes
  --   b <- nodes
  --   return $ case (SeqNetList.depends bindings' a b) of
  --     True -> Just (a,b)
  --     False -> Nothing


x_mt = do
  putStrLn "-- x_mt"
  MT.test
  
x_digiview = do
  putStrLn "-- x_digiview"
  traverse print $
    expandVC 5 [(5,   "a"),
                (10,  "b"),
                (25,  "c"),
                (100, "d")]
