{-# LANGUAGE BangPatterns #-}

-- Support for TechTools DigiView DV3100 compressed CSV data format.
module Data.AsmTools.DigiView where

-- This is based on parsec, and really not all that fast.
import Data.AsmTools.CSV

import Data.List.Split

-- Attempt to hack together a faster import.  Files can get large,
-- especially when not compressed.
readCSVFile' filename = do
  str <- readFile filename
  let (header:table) = map (splitOn ",") $ lines str
  return (header, table)


-- Prouce a time-tagged expanded
readDigiViewFile filename = do
  str <- readFile filename
  (names, table) <- readCSVFile filename
  --print (names, length table)
  let read' = read :: String -> Int
      -- First column has sequence number, we don't need it.
      !compressed = map ((map read') . tail) table
      tagged = map (\(t:vs) -> (t,vs)) compressed
  return (drop 2 names, tagged)
  
