-- (DESC) Value Change Dump
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Data.AsmTools.VCD where

import Data.Maybe
import Data.List
import Control.Applicative
import Numeric
import Data.Char

import qualified Data.ByteString.Lazy as ByteString
import Data.Binary.Get

-- https://en.wikipedia.org/wiki/Value_change_dump
tags :: [Char]
tags = map toEnum [37..126]
-- tags = map toEnum [33..126]

newtype VCD = VCD { unVCD :: String }
instance Show VCD where show = unVCD

toVCD :: (Eq a, Show a, Integral a) => String -> ([(String, Int)], [[a]]) -> VCD
toVCD timescale (types, signals) = VCD $ header ++ vars ++ vcd ++ end where
  n = length header
  header = concat $ [
    "$date October 21, 2015 $end\n",
    "$version VCD.hs $end\n",
    "$timescale " ++ timescale ++ "1ns $end\n",
    "$scope module logic $end\n"]
  vars = concat $ concat $ [
    ["$var wire ", show width, " ", [tag], " ", name, " $end\n"] |
      ((name, width), tag) <- zip types tags]
  end = "#" ++ (show $ length signals) ++ "\n"
  
  -- bits vs ints
  fmt n = (map (fmt' . snd) types) !! n
  fmt' 1 = show
  fmt' _ = binary
  binary n = "b" ++ showIntAtBase 2 intToDigit n "" ++ " "

  -- convert diff rep to strings
  vcd = concat $ map time $ diff signals
  time (t,bus) = "#" ++ show t ++ "\n" ++ (concat $ map channel bus)
  channel (c,v) = (fmt c v) ++ [tags !! c] ++ "\n"
         
-- Compute one channel's changes.
changes :: Eq a => [a] -> [Maybe a]
changes l@(l0:l') = Just l0 : zipWith f l l' where
  f old new = case new == old of
                True -> Nothing
                False -> Just new

-- Tag with index, then filter Just
tag :: [Maybe a] -> [(Int, a)]
tag = catMaybes . (zipWith (\n -> fmap (n,)) [0..])


-- Differential encoding with time and channel tags.
diff :: Eq a => [[a]] -> [(Int,[(Int,a)])]
diff signals = diff' where

  -- It's easier to operate on each channel separately.
  diffs = transpose $ map changes $ transpose signals

  -- Tag and filter on channel and time levels
  diff' = tag $ map (something . tag) diffs

  -- Adapter for use with tag
  something [] = Nothing
  something l  = Just l


-- Convert raw binary to single channel VCD file
fromByteString :: ByteString.ByteString -> VCD
fromByteString bytes = vcd where
  vcd = toVCD "1 ns" ([("data",8)], map (:[]) ints)
  ints = runGet get bytes
  nb_bytes = ByteString.length bytes
  get = do
    bs <- sequence $ [getWord8 | _ <- [1..nb_bytes]]
    return $ map fromIntegral bs

