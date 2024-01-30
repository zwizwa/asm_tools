-- Example top level file

-- This file can produce a couple of file types:
--
-- 1. Top level HDL as MyHDL .py or (later) RTL Verilog .v with
--    abstract port names.
--
-- 2. A PCF file mapping the top level HDL port names to iCE40 FPGA
--    package pins.
--
-- 3. Embedded CPU binary code
--
-- As a guideline, the top level file should do only instantiation,
-- placing the functionality in some .hs library.  Note that the style
-- is different for top level files: while .hs circuit library code is
-- applicative (state machines applied to inputs, producing outputs),
-- the HDL code is written in "network style", where output ports are
-- passed into the module as named entities, and are assigned to
-- explicitly in the body of the code.


{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE NoMonadFailDesugaring #-}

module Language.Seq.Examples.FSoc where

import Language.Seq
import Language.Seq.Lib
import Language.Seq.CPU
import Language.Seq.Names
import Language.Seq.Test.TestTools
import qualified Language.Seq.Forth as Forth
import qualified Language.Seq.MyHDL as MyHDL
import qualified Language.Seq.Verilog as Verilog

import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Expr as SeqExpr 
import qualified Data.AsmTools.CSV as CSV

import qualified Data.AsmTools.Make

import Control.Monad hiding (forever)


-- See Data.AsmTools.Makefile

-- The "targets" table is essentially a Makefile, which instead of
-- calling shell script will call into Haskell code.  It will be
-- renderable as an actual Makefile, with rules calling into the
-- generator binary.


-- main = Data.AsmTools.Make.build "f_soc" Langauge.Seq.Examples.FSoc.targets

f_soc_v = Verilog.fpgaVerilog "f_soc" f_soc

f_soc_pcf csv = do
  board <- CSV.readTagged id csv
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
  return $ Verilog.fpgaPCF f_soc pin


busywait = do for' 50 $ for' 255 $ for' 255 $ nop

f_soc_prog3_ram = packProgram $ Forth.compile prog3
f_soc_prog4_ram = packProgram $ Forth.compile prog4

prog3  =
  forever $ do
    push 0x55 ; write dbg_addr ; busywait
    push 0xAA ; write dbg_addr ; busywait

prog4  =
  forever $ do
    push 0x55 ; write dbg_addr ; busywait ; busywait
    push 0xAA ; write dbg_addr ; busywait ; busywait


targets =
  let 
    writePCF ([pcf], [csv]) = do
      ob <- f_soc_pcf csv
      writeFile pcf $ show ob
      
    writeProg file  = (writeProgram file) . Forth.compile


  in [
    ((["f_soc.v"], []),
      \([f],[]) -> writeFile f $ show $ f_soc_v),

    ((["f_soc.breakout.pcf"], ["specs/f_soc.breakout.csv"]), writePCF),
    ((["f_soc.fbr.pcf"],      ["specs/f_soc.fbr.csv"]),  writePCF),

    ((["f_soc.prog1.bin"], []),
      \([f],[]) -> writeProg f $ do
        push 0x55 ; write dbg_addr ; begin ; again),

    ((["f_soc.prog2.bin"], []),
      \([f],[]) -> writeProg f $ do
        begin ; push 0 ; again),

    ((["f_soc.prog3.bin"], []), 
      \([f],[]) -> writeProg f prog3)
    ]

-- FIXME: .py
-- # # f_%.py f_%.pcf f_%.imem.bin: f_%.hs $(HS)
-- # # 	rm -f f_$*.imem.bin # workaround: openBinaryFile: resource exhausted (Resource temporarily unavailable)
-- # # 	$(NIX_SHELL) --run "./cabal.sh test f_$* --log=/dev/stdout"


-- For HX8K breakout, on-board clock is 12MHz


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
      [tx]     <- withProbe ["bus_data"] bus_data $
                  withProbe ["bus_dbg"]  bus_dbg  $
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






