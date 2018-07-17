{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

import Seq
import SeqLib
import SeqEmu
import Data.Bits
import Data.List
import Data.List.Split

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
import Test.QuickCheck hiding ((.&.),(.|.))

qc str f = do
  putStrLn $ "-- " ++ str
  quickCheck f
  
main = do
  putStrLn "clocked_shift'"
  print $ clocked_shift' 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0]]
  qc "p_bits" p_bits
  qc "p_sample" p_sample
  -- qc "p_clocked_shift" p_clocked_shift

-- General notes.
--
-- Sequences of equal length are simplest to generate as a single
-- sequence of tuples.
  

-- downSample is left inverse of upSample
-- Test this separately as it is used in other tests.
p_sample :: [(NonNegative Int, Int)] -> Bool
p_sample spec = seq == seq' where
  spaces = map (getNonNegative . fst) spec
  seq    = map snd spec
  seq'   = downSample $ upSample spaces seq


-- Behavioral tests for SeqLib functions.
p_clocked_shift :: Positive Int -> [(NonNegative Int, Int)] -> Bool
p_clocked_shift (Positive nb_bits) spec = p1 where
  
  p1 = wordSeq == wordSeq'

  spaces  = map (getNonNegative . fst) spec
  spaces' = concat $ replicate nb_bits spaces
  wordSeq = map ((mask nb_bits) . snd) spec

  bitSeq   = toBitss nb_bits wordSeq
  ins      = upSample spaces' bitSeq
  outs     = f ins
  wordSeq' = downSample outs

  -- test for the test
  f = (upSample $ cycle [1]) . (toWords nb_bits) . downSample
  

-- Tools
toBits :: Int -> Int -> [Int]
toBits nb_bits n = map (shiftL 1) [0..nb_bits-1]

toBitss :: Int -> [Int] -> [Int]
toBitss nb_bits = concat . (map $ toBits nb_bits)

toWord :: [Int] -> Int
toWord bits = foldr f 0 $ reverse bits where
  f bit accu = (bit .&. 1) .|. (shiftL accu 1)

toWords :: Int -> [Int] -> [Int]
toWords nb_bits = (map toWord) . (chunksOf nb_bits)

p_bits :: Positive Int -> [Int] -> Bool
p_bits (Positive nb_bits) words = p1 && p2  where
  p1 = words == words'
  p2 = bits  == bits'
  bits   = toBitss nb_bits words
  words' = toWords nb_bits bits
  bits'  = toBitss nb_bits words'


mask nb_bits v = v .&. msk where
  msk = (1 `shiftL` nb_bits) - 1


-- onInts wrappers for SUTs
--
-- To keep things simple, use lists for all input/output busses.  The
-- wrappers below contain type specs for inputs.
trace ::
  [Int]
  -> ([SeqEmu.R S] -> SeqEmu.M [SeqEmu.R S])
  -> [[Int]]
  -> [[Int]]
trace typs fm ins =
  iticks (onInts typs fm) ins

clocked_shift' nb_bits = trace [1,1] $ \[bc,bv] -> do
  (wc, wv) <- clocked_shift (SInt (Just nb_bits) 0) (bc, bv)
  return [wc, wv]
