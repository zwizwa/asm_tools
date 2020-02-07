-- PcbNew netlist parser

-- Ad hoc, based on KiCad output

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

-- module EDIF(readEdifFile,readEdif,paths,libraries,nodeName) where
module Data.AsmTools.PcbNew where

-- Represent as bare-bones S-Expression (Free [] String)
import Data.AsmTools.SE


import Control.Monad.Free
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as Map

testPcbNew = do
  -- KiCad Eeschema : export netlist as Pcbnew
  let fileName = "/home/tom/constell8/bob/KLSTR Interface Board.net"
  h <- readFile fileName
  case readSE fileName h of
    Left msg ->
      putStrLn msg
    Right se -> do
      traverse print $ nets se
      traverse print $ components se
      -- print $ head $ components se
      return ()

---- Relevant structre:
--
-- (export (version D)
--   (design ...)
--   (components
--      (comp ...) ...)
--   (libparts
--     (libpart ..) ...)
--   (libraries
--     (library ...) ...)
--   (nets
--     (net
--       (code 1)
--       (name /PD11_SPIO_MOSI)
--       (node (ref J15) (pin A9))
--       (node (ref J8)  (pin 7)))
--     ...))

-- Some ad-hoc accessors.

-- For top level expression
refExport (Free ((Pure "export") : dict)) = refDict dict where

-- For dictionaries, ((tag val) ...)
-- Returns multiple maches
refDict dict name = rv where 
  rv = catMaybes $ map collect dict
  collect (Free ((Pure n) : contents)) =
    if n == name then Just contents else Nothing

-- Nothing to string
refDict' dict name = rv where
  [[Pure rv]] = refDict dict name


-- Parse netlist
nets se = map parseNet $ head $ refExport se "nets" where
  parseNet (Free ((Pure "net") : netDict)) = (name,ns) where
    name = refDict' netDict "name"
    ns = map node $ refDict netDict "node"
    node nodeDict = (refDict' nodeDict "ref", refDict' nodeDict "pin")


components se = map parseComp $ head $ refExport se "components" where
  parseComp (Free ((Pure "comp") : compDict)) = (ref,value) where
    ref   = refDict' compDict "ref"
    value = refDict' compDict "value"
--   (components
--      (comp ...) ...)


