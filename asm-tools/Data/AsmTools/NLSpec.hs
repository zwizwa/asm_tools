-- (DESC) Netlist specifications + tests

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

-- module EDIF(readEdifFile,readEdif,paths,libraries,nodeName) where
module Data.AsmTools.NLSpec
  (testNLSpec)
where

-- Represent as bare-bones S-Expression (Free [] String)
import Data.AsmTools.SE


import Control.Monad.Free
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as Map

testNLSpec = do
  -- KiCad Eeschema : export netlist as Pcbnew
  let fileName = "/home/tom/constell8/bob/node-checklist.nlspec"
  h <- readFile fileName
  case readSE fileName h of
    Left msg ->
      putStrLn msg
    Right se -> do
      case tables se of
        Left e ->
          putStrLn e
        Right ts ->
          do
            putStrLn "tables:"
            traverse print ts
            return ()
      case rules se of
        Left e ->
          putStrLn e
        Right rs ->
          do
            putStrLn "rules:"
            traverse print rs
            return ()


--parse :: SE -> M ([String],[String])
tables expr = do
  l <- tagged "nlspec" expr
  ts <- filter_tag "table" l
  traverse table ts
  
table expr = do
  rows <- tagged "table" expr
  traverse row rows

row expr = do
  cells <- list expr
  traverse cell cells

cell expr = do
  string expr



rules expr = do
  l <- tagged "nlspec" expr
  rs <- filter_tag "with-connection" l
  return rs

  

{-
Free [Pure "with-connection",Free [Free [Pure "J1",Pure "1"],Free [Pure "J1",Pure "2"]],Free [Pure "conected",Pure "BP_TX",Pure "KLSTR_RX"]]
-}
