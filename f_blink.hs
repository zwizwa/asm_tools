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

  
main = do
  test
  generate

test = do
  return ()

-- Blink-a-LED example for HX8K breakout
-- On-board clock is 12MHz
f_blink :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_blink =
  $(named
   [|
    \[ _LED0 ] -> do
      c <- counter $ (bits 24)
      led <- slice' c 23 22
      connect _LED0 led
    |])
  
generate = do
  putStrLn "-- f_blink"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
  MyHDL.fpgaWrite "f_blink" f_blink pin


