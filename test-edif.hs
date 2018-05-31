{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


import qualified EDIF as EDIF
import System.Environment
-- import Seq
-- import SeqLib
-- import qualified SeqTerm
-- import qualified SeqExpr
-- import qualified SeqEmu
-- import qualified MyHDL
-- import qualified CPU
-- import Data.Map.Lazy (empty, foldrWithKey, insert)
-- import qualified Data.Map.Lazy as Map
-- import qualified Control.Applicative as Applicative
-- import Control.Applicative (ZipList(..))
-- import Data.Functor.Apply

  
main = getArgs >>= main'

main' [] = do
  main' ["/tmp/test.edif"]
main' [fileName] = do
  contents <- readFile fileName
  case EDIF.readEDIF fileName contents of
    Left errorMsg ->
      putStrLn errorMsg
    Right edif ->
      print edif
    

-- List structure seems fine.  Focus on atoms.
