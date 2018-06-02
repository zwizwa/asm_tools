{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


import EDIF
import System.Environment
import Data.Foldable
import Control.Monad.Free
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

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
  contents <- EDIF.readEdifFile fileName
  case readEdif fileName contents of
    Left errorMsg ->
      putStrLn errorMsg
    Right edif ->
      -- printl $ map noTags $ mapToList $ EDIF.paths edif
      -- printl $ mapToList $ EDIF.paths edif
      print $ map nodeName $ libraries edif

noTags (p, a) = (map snd p, a)

mapToList :: Map k v -> [(k, v)]
mapToList = Map.foldrWithKey f [] where f k v t = (k,v):t
      

printl es = sequence_ $ map print es
