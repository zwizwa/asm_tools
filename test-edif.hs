{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


import EDIF
import System.Environment
import Data.Foldable
import Control.Monad.Free

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
  contents <- EDIF.readFile fileName
  case EDIF.readEDIF fileName contents of
    Left errorMsg ->
      putStrLn errorMsg
    Right edif ->
      -- List structure seems fine.  Focus on atoms.
      -- print edif
      -- printl $ toList edif
      -- putStr $ EDIF.show' edif
      -- printl $ EDIF.paths [Edif,Library,Cell,View,Contents] edif
      printl $ toList $ EDIF.rels edif
      

printl es = sequence_ $ map print es

      -- Edif:Library:Cell:View:Contents:Net:Joined:PortRef:InstanceRef:sub ->
      -- Edif:Library:Cell:View:Contents:Net:Joined:PortRef:sub ->
      -- Edif:Library:Cell:View:Contents:Net:sub ->
      -- Edif:Library:Cell:View:Contents:sub ->

        
        -- &6/
        -- InstanceRef/
        --   P3/
        -- InstanceRef/
