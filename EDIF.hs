-- EDIF netlist parser

-- Ad hoc, based on Altium output.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

-- module EDIF(readEdifFile,readEdif,paths,libraries,nodeName) where
module EDIF where

-- Represent as bare-bones S-Expression (Free [] String)
import SE
import Control.Monad.Free

import System.IO
import qualified Data.Map.Strict as Map


-- Altium files are latin1.
readEdifFile fileName = do
  h <- openFile fileName ReadMode
  hSetEncoding h latin1
  hGetContents h




-- Queries.

-- There are basically two ways to approach this:

-- a) Top-down: match / query starting at the top of the hierarchy
-- b) Bottom-up: filter nodes from zipper paths

-- Use the former.  It exposes tedious boilerplate but more readable
-- and easier to cross-reference with an example data file.  The
-- alternative bottom up code left after that, for reference.



-- Matching is ad-hoc, based on Altium export.  Might need some extra
-- cases.  This uses named subnode filtering based on sub,sub'

-- (library SHEET_LIB (cell (view netListView (contents ...))))
contents edif = cont where
  --             node  node_tag    node_name
  --------------------------------------------------
  [sheet] = sub' edif  "library"   "SHEET_LIB"
  [cell]  = sub  sheet "cell"
  [view]  = sub' cell  "view"      "netListView"
  [cont]  = sub  view  "contents"

netlist edif = table where
  grouped = map net nets
  table = concat $ map (\(n, ps) -> map (\p -> (n, p)) ps) grouped
  
  nets    = sub (contents edif) "Net"

  net n@(Free ((Pure "Net"):name:_)) = (name',ports) where
    name' = rename name
    [js]  = sub n "Joined"
    prefs = sub js "PortRef"
    ports = map joined prefs

  joined (Free [(Pure "PortRef"),p,i]) = (port p, inst i)

  port (Pure p) = p
  inst (Free [(Pure "InstanceRef"),(Pure i)]) = i
  rename (Pure n) = n
  rename (Free [(Pure "rename"),(Pure n1),(Pure n2)]) = n2

-- (contents (Instance <name> (viewRef NetlistView (cellRef <cell> (LibraryRef <lib>)) ..)) ..)
instances edif = map inst insts where
  insts   = sub (contents edif) "Instance"
  inst n@(Free ((Pure "Instance"):(Pure name):_)) = (name, (cell, lib)) where
    -- Are there other patterns to support?
    [vr] = sub' n  "viewRef" "NetlistView"
    (Free
     [(Pure "viewRef"),
      (Pure "NetlistView"),
       (Free [(Pure "cellRef"),
              (Pure cell),
               (Free
                [(Pure "LibraryRef"),
                 (Pure lib)])])]) = vr
          



  



-- HACKS

-- Alternative implementation using bottom scraper.  This is left here
-- for illustration.  The general approach leads to concise code but
-- is very dirty in this case because structure might not be rigid.
-- Also, not very readable.  It's basically hard-to-read c[a|d]r
-- programming. The use of retract is just a notational short-hand.

netlist' :: SE -> [(String,(String,String))]
netlist' edif = map rel irefs' where

  -- Fish out a key that produces a single node as an anchor point to
  -- retrieve information..
  irefs' = scrape "InstanceRef" edif

  -- .. and fetch the rest from the based on relative coordinates.
  -- ("U8",[("InstanceRef",1),("PortRef",2),("Joined",1),("Net",2) ...
  --                  ("A23",[("PortRef",1),("Joined",1),("Net",2) ...
  --                                       ("SPI0_SCLK",[("Net",1) ...
  rel (1:2:j:2:p) = (net,(inst,port)) where
    inst = ref (1:2:j:2:p)
    port = ref   (1:j:2:p)
    net  = ref       (1:p)

  ref :: [Int] -> String
  ref = rename . retract . (refSE edif) -- (1)

  -- Handle node rename forms.  Not sure how this works..
  rename [v] = v
  rename ["rename", n1, n2] = n2

instances' edif = map rel irefs' where
  irefs' = scrape "Instance" edif
  ref = rename . retract . (refSE edif) -- (1)
  rel (1:p) = (inst, typ) where
    inst = ref     (1:p)
    typ  = ref (1:2:2:p)
  rename [v] = v

-- Bottom scraper
scrape :: String -> SE -> [[Int]]
scrape name edif = map (map snd) $ Map.keys $ Map.filterWithKey f $ paths edif where
  f ((name',_):_) _ = name == name'
  f _ _ = False
  

