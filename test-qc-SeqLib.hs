{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

import Seq
import SeqLib
import SeqEmu

-- import Data.Map.Lazy (empty, foldrWithKey, insert)
-- import qualified Data.Map.Lazy as Map
-- import qualified Control.Applicative as Applicative
-- import Control.Applicative (ZipList(..))
-- import Control.Category
-- import Prelude hiding((.),id)
-- import Control.Arrow
-- import Control.Monad
-- import Data.List
-- import Data.Key(Zip(..),zipWith)
-- import Data.Typeable
import Test.QuickCheck

qc str f = do
  putStrLn $ "-- " ++ str
  quickCheck f
  
main = do
  qc "prop_sample" prop_sample

prop_sample :: [(NonNegative Int, Int)] -> Bool
prop_sample spec = seq == seq' where
  spaces = map (getNonNegative . fst) spec
  seq    = map snd spec
  seq'   = downSample $ upSample spaces seq




