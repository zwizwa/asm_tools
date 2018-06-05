module VCD where

import Data.Maybe
import Data.List
import Control.Applicative

tags = "#$%&'()"

-- FIXME: binary only

toVCD :: (Eq a, Show a) => [(String,a)] -> [[a]] -> String
toVCD types signals = header ++ vars ++ vcd where
  n = length header
  header = concat $ [
    "$date November 11, 2009 $end\n",
    "$version VCD.hs $end\n",
    "$timescale 1ns $end\n",
    "$scope module logic $end\n"]
  vars = concat $ concat $ [
    ["$var wire ", show width, " ", [tag], " ", name, " $end\n"] |
      ((name, width), tag) <- zip types tags]

  vcd = concat $ map time $ diff signals
  time (t,bus) = "#" ++ show t ++ "\n" ++ (concat $ map channel bus)
  channel (c,v) = show v ++ [tags !! c] ++ "\n"
         
--  changes = concat $ map frame $ diff signals
--  frame (time, bus) = _ where
    
  

-- Compute one channel's changes.
changes :: Eq a => [a] -> [Maybe a]
changes l@(l0:l') = Just l0 : zipWith f l l' where
  f old new = case new == old of
                True -> Nothing
                False -> Just new

-- Tag with index, then filter Just
tag :: [Maybe a] -> [(Int, a)]
tag = catMaybes . (zipWith f [0..]) where
  f n m = liftA2 (,) (return n) m

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
