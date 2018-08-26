-- Example top level file for generating .py and .pcf files

-- Only used setting up I/O.
-- Any non-trivial functionality should go into a library.

{-# LANGUAGE TemplateHaskell #-}

import Language.Seq
import Language.Seq.Lib
import Language.Seq.CPU
import Language.Seq.Names
import Language.Seq.Test.Tools
import qualified Language.Seq.MyHDL as MyHDL
import qualified Language.Seq.Verilog as Verilog

import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Expr as SeqExpr
import qualified Data.AsmTools.CSV as CSV

  
main = do
  generate

-- Blink-a-LED example for HX8K breakout
-- On-board clock is 12MHz
f_blink :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_blink =
  $(named
   [|
    \[ _LED0 ] -> do

      -- The blink-a-led
      c <- counter $ (bits 24)
      led <- slice' c 23 22
      connect _LED0 led

      
      

      
    |])
  
generate = do
  putStrLn "-- f_blink"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
  -- MyHDL.fpgaWrite "f_blink" f_blink pin
  Verilog.fpgaWrite "f_blink" f_blink pin


