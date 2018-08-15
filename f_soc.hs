-- Example top level file for generating .py and .pcf files

-- Only used setting up I/O.
-- Any non-trivial functionality should go into a library.

{-# LANGUAGE TemplateHaskell #-}

import Seq
import SeqLib
import CPU
import Names
import TestTools
import qualified MyHDL

import qualified SeqTerm
import qualified SeqExpr
import qualified CSV

import Control.Monad
  
main = do
  test
  generate

test = do
  return ()

-- SOC example for HX8K breakout
-- On-board clock is 12MHz

-- tom@tp:/i/tom/asm_tools$ echo -ne 'U' >/tmp/test.bin ; iceprog -x /tmp/test.bin


f_soc :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_soc =
  $(named
   [|
    \[ _RX, _TX,
       _SPI_SI, _SPI_SCK, _SPI_SS_B,
       _LED0, _LED1, _LED2, _LED3,
       _LED4, _LED5, _LED6, _LED7
     ] -> do
      let sample = delay >=> delay
      [rx, spi_si, spi_sck, spi_ss_b] <-
        sequence $ map sample [_RX, _SPI_SI, _SPI_SCK, _SPI_SS_B]

      (uart_rx_e, uart_rx) <- async_receive 8 rx
      (spi_rx_e,  spi_rx)  <- sync_receive 8 spi_ss_b spi_sck spi_si

      dbg <- closeReg [bits 8] $ \[dbg] -> do
        dbg' <- if' spi_rx_e spi_rx dbg
        return ([dbg'],dbg)
      
      -- b0 <- slice' uart_rx 1 0
      -- connect _LED0 b0
      let leds = [_LED0, _LED1, _LED2, _LED3,
                  _LED4, _LED5, _LED6, _LED7]
          connDbg n pin = slice' dbg (n+1) n >>= connect pin
          connLEDs = sequence_ $ zipWith connDbg  [0..] leds

      connLEDs
      
      -- Instantiate the SOC
      [tx] <- soc [_RX]
      connect _TX tx
    |])

generate = do
  putStrLn "-- f_soc"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
  MyHDL.fpgaWrite "f_soc" f_soc pin


