-- Example top level file for generating .py and .pcf files

-- Only used setting up I/O.
-- Any non-trivial functionality should go into a library.

{-# LANGUAGE TemplateHaskell #-}

import Language.Seq
import Language.Seq.Lib
import Language.Seq.CPU
import Language.Seq.Names
import Language.Seq.Test.Tools
import qualified Language.Seq.Forth as Forth
import qualified Language.Seq.MyHDL as MyHDL
import qualified Language.Seq.Verilog as Verilog

import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Expr as SeqExpr 
import qualified Data.AsmTools.CSV as CSV

import Control.Monad hiding (forever)
  
main = do
  test
  generate

test = do
  return ()

-- SOC example for HX8K breakout
-- On-board clock is 12MHz


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


      -- Baud rate generator.
      -- Bit size is set here to work around a Verilog.hs bug
      let tx_bc = 1
      -- Instantiate the SOC with dbg probe.  FIXME:

      -- FIXME: Probably ok for debug, but these are
      -- double-registered.  Change 'withProbe' to allow enables or to
      -- contain some Dynamic type, or allow combinatorial Connect.
      bus_data <- signal $ bits 8
      bus_dbg  <- signal $ bit
      dbg      <- register bus_dbg bus_data
      [tx]     <- withProbe "bus_data" bus_data $
                  withProbe "bus_dbg"  bus_dbg  $
                  soc [rx, tx_bc, cs, sck, sda]
    
      -- Instantiate the SOC
      -- iw  <- signal $ bits 16
      -- dbg <- slice' iw 16 8
      -- dbg <- signal $ bits 8
      -- [tx, dbg'] <- withProbe "iw" iw $ soc [rx, cs, sck, sda]

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
      c = Forth.compile
      busywait = do for' 100 $ for' 255 $ for' 255 $ nop
      prog1 = c $ do push 0x55 ; write dbg ; begin ; again
      prog2 = c $ do begin ; push 0 ; again
      prog3 = c $ forever $ do
        push 0x55 ; write dbg ; busywait
        push 0xAA ; write dbg ; busywait

  MyHDL.fpgaWrite "f_soc" f_soc pin
  -- Verilog2.fpgaWrite "f_soc" f_soc pin
  
  writeProgram "f_soc.imem.bin" prog3


