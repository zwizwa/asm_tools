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

-- SOC example for HX8K breakout
-- On-board clock is 12MHz

f_soc :: ([String], [SeqTerm.R S] -> SeqTerm.M ())
f_soc =
  $(named
   [|
    \[ _RX, _TX,
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
      
      [tx] <- soc [_RX]
      connect _TX tx
    |])
generate = do
  putStrLn "-- f_soc.py, f_soc.pcf"
  board <- CSV.readTagged id "specs/hx8k_breakout.csv"
  let pin = CSV.ff (\[k,_,v,_] -> (k,v)) board
      (py,pcf) = MyHDL.fpgaGen "f_soc" f_soc pin
  writeFile "f_soc.py" $ show py
  writeFile "f_soc.pcf" $ show pcf


