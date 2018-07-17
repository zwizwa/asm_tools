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
import Test.QuickCheck hiding ((.&.),(.|.))

qc str f = do
  putStrLn $ "-- " ++ str
  quickCheck f
  
main = do
  -- print $ toWord [1,0,0,0]
  -- print $ toBits 4 8
  -- print $ downSample' $ clocked_shift' 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0,0,0,0,1]]
  qc "p_bits" p_bits
  qc "p_sample" p_sample
  qc "p_clocked_shift" p_clocked_shift

-- General notes.
--
-- Sequences of equal length are simplest to generate as a single
-- sequence of tuples.
  

-- downSample is left inverse of upSample
-- Test this separately as it is used in other tests.
p_sample :: [(NonNegative Int, Int)] -> Bool
p_sample spec = seq == seq' where
  spaces = map (getNonNegative . fst) spec
  seq    = map ((:[]) . snd) spec
  seq'   = downSample' $ upSample' spaces seq


-- Behavioral tests for SeqLib functions.
p_clocked_shift :: NonNegative Int -> Positive Int -> [Int] -> Bool
p_clocked_shift (NonNegative sub) (Positive nb_bits) ints = p1 where

  p1 = wordSeq == wordSeq'

  -- FIXME: It would be nice to have range generators
  wordSeq  = map (mask nb_bits) ints
  bitSeq   = toBitss nb_bits wordSeq
  ins      = upSample' (cycle [sub `rem` 5]) $ map (:[]) bitSeq
  outs     = clocked_shift' nb_bits ins
  wordSeq' = map head $ downSample' outs





-- Tools

-- Defaults use "natural bit order", which places MSB on the left,
-- which makes list form, scope display and normal digit display.

toBits :: Int -> Int -> [Int]
toBits nb_bits val = map ((.&. 1) . (shiftR val)) $ reverse [0..nb_bits-1]

toBitss :: Int -> [Int] -> [Int]
toBitss nb_bits = concat . (map $ toBits nb_bits)

toWord :: [Int] -> Int
toWord bits = foldr f 0 $ reverse bits where
  f bit accu = (bit .&. 1) .|. (shiftL accu 1)

toWords :: Int -> [Int] -> [Int]
toWords nb_bits = (map toWord) . (chunksOf nb_bits)

p_bits :: Positive Int -> [Int] -> Bool
p_bits (Positive nb_bits) ints = p1 && p2  where
  
  p1 = words == words'
  p2 = bits  == bits'

  words  = map (mask nb_bits) ints
  bits   = toBitss nb_bits words
  words' = toWords nb_bits bits
  bits'  = toBitss nb_bits words'





mask nb_bits v = v .&. msk where
  msk = (1 `shiftL` nb_bits) - 1


-- Bus sequences.

-- To keep things simple, use [] as signal container for input and
-- output busses.

-- In that setting, subsampled signals are simplest to represent by
-- pushing/poping the enable bit to/from the bus list.

upSample' :: [Int] -> [[Int]] -> [[Int]]
upSample' spaces = upSample en spaces where
  en True  a = (1:a)
  en False a = (0:a)

downSample' :: [[Int]] -> [[Int]]
downSample' = downSample sel where
  sel (1:a) = Just a
  sel (0:a) = Nothing

-- onInts wrappers for SUTs
trace ::
  [Int]
  -> ([R S] -> M [R S])
  -> [[Int]] -> [[Int]]
trace inputBitSizes fm ins =
  iticks (onInts inputBitSizes fm) ins

clocked_shift' nb_bits = trace [1,1] $ \[bc,bv] -> do
  (wc, wv) <- clocked_shift (SInt (Just nb_bits) 0) (bc, bv)
  return [wc, wv]
