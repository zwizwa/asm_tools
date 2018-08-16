-- Example top level file for generating .py and .pcf files

-- Only used setting up I/O.
-- Any non-trivial functionality should go into a library.

{-# LANGUAGE TemplateHaskell #-}

import Seq
import SeqLib
import CPU
import Names
import TestTools
import qualified Forth
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
      [rx, sda, sck, cs] <-
        sequence $ map sample [_RX, _SPI_SI, _SPI_SCK, _SPI_SS_B]

      -- Instantiate the SOC
      -- iw  <- signal $ bits 16
      -- dbg <- slice' iw 16 8
      dbg <- signal $ bits 8
      
      [tx, dbg'] <- withProbe "ip" dbg $
        soc [rx, cs, sck, sda]

      -- (spi_rx_e,  spi_rx)  <- sync_receive 8 spi_ss_b spi_sck spi_si
      -- dbg <- register spi_rx_e spi_rx

      connect _TX tx
      
      let leds = [_LED0, _LED1, _LED2, _LED3,
                  _LED4, _LED5, _LED6, _LED7]
          connDbg n pin = slice' dbg (n+1) n >>= connect pin
          connLEDs = sequence_ $ zipWith connDbg  [0..] leds

      connLEDs
      -- connect _LED5 sda
      -- connect _LED6 sck
      -- connect _LED7 cs
      
    |])

generate = do
  putStrLn "-- f_soc"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
      prog' = Forth.compile $ do push 0x55 ; write dbg
      prog = Forth.compile $ do nop ; nop ; nop ; nop ; begin ; again
  MyHDL.fpgaWrite "f_soc" f_soc pin
  writeProgram "f_soc.imem.bin" prog


