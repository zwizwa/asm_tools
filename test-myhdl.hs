{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}


import Seq
import SeqLib
import CPU
import Names
import TestTools
import qualified MyHDL

import qualified SeqTerm
import qualified SeqExpr
import qualified CSV


  
main = do

  x_ifs
  
  x_hdl
  x_hdl_sync
  
  x_blink_fpga
  x_uart_fpga
  x_soc_fpga


    
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



-- SOC
f_soc_fpga :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_soc_fpga =
  $(named
   [|
    \[ _RX, _TX,
       _LED0, _LED1, _LED2, _LED3,
       _LED4, _LED5, _LED6, _LED7
     ] -> do
      [tx] <- soc [_RX]
      connect _TX tx
    |])

x_soc_fpga = do
  putStrLn "-- x_soc_fpga"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
      (py,pcf) = MyHDL.fpga' "x_soc_fpga" f_soc_fpga pin
  writeFile "x_soc_fpga.py" $ show py
  writeFile "x_soc_fpga.pcf" $ show pcf



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
  


