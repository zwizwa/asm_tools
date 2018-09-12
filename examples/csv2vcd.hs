{-# LANGUAGE BangPatterns #-}

import Data.AsmTools.DigiView
import Data.AsmTools.VCD
import System.Environment
import Language.Seq.Test.Tools


main = do
  [infile, outfile] <- getArgs
  info@(names, tagged) <- readDigiViewFile infile
  let signals = map snd $ expandVC 5 tagged
      types = [(name, 1) | name <- names]
      write ts = writeFile outfile $ show $ toVCD "5ns" ts
      spi [n] = toBits 4 [n]

  print types
  case types of
    [("SPI",1)] -> do
      putStrLn "Splitting SPI lines"
      -- SPI lines are exported as a number.
      let signals' = map spi signals
          types' = [(n,1) | n <- ["D0","D1","CS","CLK"]]
      write (types', signals')
    _ -> do
      print types
      write (types, signals)
  
