{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


import EDIF
import SE
import CSV
import Protel
import System.Environment
import Data.Foldable
import Control.Monad.Free
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Tuple

main = getArgs >>= main'

main' [] = do
  main' ["/tmp/test.edif"]
main' [fileName] = do
  contents <- EDIF.readEdifFile fileName
  case readSE fileName contents of
    Left errorMsg ->
      putStrLn errorMsg
    Right edif -> do
      -- printl $ map noTags $ mapToList $ EDIF.paths edif
      -- print $ map nodeName $ libraries edif
      -- printl $ map swap $ mapToList $ EDIF.paths edif
      
      -- putStrLn "-- netlist"
      -- printl $ EDIF.netlist edif
      -- putStrLn "-- instances"
      -- printl $ EDIF.instances edif

      -- putStrLn "-- paths"
      -- printl $ map swap $ mapToList $ EDIF.paths edif

      -- printl $ EDIF.netlist' edif

      -- printl CSV.test
      test_csv
      test_protel

noTags (p, a) = (map snd p, a)

mapToList :: Map k v -> [(k, v)]
mapToList = Map.foldrWithKey f [] where f k v t = (k,v):t

printl es = sequence_ $ map print es

test_csv = do
  let f = "/tmp/test.csv"
  contents <- readFile f
  case readCSV f contents of
    Right table -> printl table
    Left msg -> error msg

-- Protel Netlist
test_protel = do
  let f = "/tmp/test.net"
  contents <- readFile f
  case readProtel f contents of
    Right (comps,nets) -> do
      putStrLn "protel components:"
      printl comps
      putStrLn "protel nets:"
      printl nets
    Left msg -> error msg
