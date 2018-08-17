{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}


import Seq
import SeqLib
import CPU
import Names
import TestTools
import qualified MyHDL
import qualified MyHDLRun

import qualified SeqTerm
import qualified SeqExpr
import qualified CSV

import qualified Verilog
  
main = do

  x_ifs
  
  x_hdl
  x_hdl_sync
  
  x_run_myhdl

  x_testbench

  x_verilog
    
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
  let (ports, bindings) = SeqTerm.compileTerm src
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
  

-- http://hackage.haskell.org/package/shelly
x_run_myhdl = do
  putStrLn "-- x_run_myhdl"
  -- MyHDLRun.run_process "x_soc_fpga"
  MyHDLRun.test


x_testbench = do
  putStrLn "-- x_testbench"
  let mod [s, d] = do return $ [s, d]
      tb = MyHDL.testbench "x_testbench" [1,8] mod [[1,0],[0,0]]
  print tb


x_verilog = do
  putStrLn "-- x_verilog"
  let mod [rx, tx] = do
        tx' <- inv rx
        connect tx tx'
  print $ Verilog.vModule "mymod" ["rx", "tx"] [bit, bit] mod
  
