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

  print (names, head tagged)
      
  writeFile outfile $ show $ toVCD "5ns" (types, signals)
  
