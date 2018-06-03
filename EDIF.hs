-- EDIF netlist parser

-- Minimally typed "scraper style".

-- LICENSE: This is derived from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
-- FIXME: currently assuming that's ok

--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE TypeFamilies #-}

-- module EDIF(readEdifFile,readEdif,paths,libraries,nodeName) where
module EDIF where

import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Free
import System.IO
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
-- import Data.Foldable
-- import GHC.Generics

-- Keep it really simple.
type Leaf = String
type Node = ([])
type EDIF = Free Node Leaf

-- Addresses are backwards paths by default.
refEdif n is = refEdif' n (reverse is)
refEdif' = f where
  f n [] = n
  f (Free nodes) (i:is) = f (nodes !! i) is



-- Parser

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseString :: Parser EDIF
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  -- Not necessary to distinguish from Atom
  return $ Pure $ x
                
parseAtom :: Parser EDIF
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Pure $ first:rest

-- Parse numbers separately, but represent them as strings.
parseNumber :: Parser EDIF
parseNumber = liftM Pure $ many1 digit               

parseExpr :: Parser EDIF
parseExpr  = spaces >> (parseAtom <|> parseNumber <|> parseString <|> parseList)
parseList  = do
  char '(' ;
  tag <- parseExpr
  args <- sepEndBy parseExpr spaces1
  spaces ; char ')'
  return $ Free (tag:args)


readEdif :: String -> String -> Either String EDIF
readEdif fileName contents =
  case parse parseExpr "lisp" contents of
    Right parsed -> Right parsed
    Left parseError -> Left msg' where
      ep = errorPos parseError
      l = show $ sourceLine ep
      -- c = show $ sourceColumn ep
      -- msg = mconcat $ fmap messageString $ errorMessages parseError
      msg = show parseError
      msg' = fileName ++ ":" ++ l ++ ": " ++ msg


-- Files are latin1.
readEdifFile fileName = do
  h <- openFile fileName ReadMode
  hSetEncoding h latin1
  hGetContents h





-- Convert parser tree output to path->leaf map
paths :: EDIF -> PathToLeaf

-- Uniqueness of paths is guaranteed by numbering nodes using the
-- original file structure.  Node type information is tagged on to
-- keep paths human-readable, and allow semantic filter operations
-- without a need for data structure lookups.
type UniqueLeaf = (Leaf, Index)

-- Paths are useful to expose the structure of a document just for
-- exploration, but also for navigation.  E.g. reversing paths turns a
-- structure "inside out".  Use "zipper" order to record the paths,
-- which seems most convenient to work with.
type UniquePath = [UniqueLeaf]


-- E..g.
-- ([("edif",6),("library",3),("cell",2),("view",3),("contents",91),("Net",0)],"SPI0_SCLK")
-- first node is an edif node, take entry 6
-- arriving at a library node, take entry 3,
-- ..
-- finally arriving at a leaf node "SPI0_SCLK"
-- ...
-- Structurally, [6,3,2,3,91,0] is enough to identify the "PI0_SCLK" entry.

-- Use a state-reader monad to construct the dictionary.
newtype PathsM t = PathsM { unPathsM :: StateT PathToLeaf (Reader UniquePath) t }
  deriving (Functor, Applicative, Monad,
            MonadState PathToLeaf, MonadReader UniquePath)
type PathToLeaf = Map UniquePath Leaf
type Index = Int

-- Zipper paths
paths edif = s where
  ((), s) = runReader (runStateT (unPathsM $ iterPathsM edif) Map.empty) []

-- Also provide normal paths
paths' edif = Map.fromList $ map f $ Map.toList $ paths edif where
  f (k,v) = (reverse k, v)

-- Explicit recursion.  Standard iterM doesn't fit the case here.  
iterPathsM :: EDIF -> PathsM ()
iterPathsM node = do
  here <- ask
  case node of
    (Pure val) -> do
      modify $ Map.insert here $ val
    (Free ((Pure tag):nodes)) -> do
      let sub node num = local (\_ -> (tag,num):here) $ iterPathsM node
      sequence_ $ zipWith sub nodes [1..]

-- Note: while Free is used for the data structure, none of its
-- properties are actually used in parsing nor path construction.




-- Queries.

-- There are basically two ways to approach this:

-- a) Top-down: match / query starting at the top of the hierarchy
-- b) Bottom-up: filter nodes from zipper paths

-- Use the former.  Tedious but more readable.  Alternative bottom up
-- code left after that, for reference.



-- Given node, find all subnodes with a particular node type tag.
sub :: EDIF -> Leaf -> [EDIF]
sub (Free (_:ns)) t = filter f ns where
  f (Free ((Pure t'):_)) = t == t'
  f _ = False

-- Same, but for the 2-tag pattern: node type, node name
sub' :: EDIF -> Leaf -> Leaf -> [EDIF]
sub' (Free (_:ns)) t1 t2 = filter f ns where
  f (Free ((Pure t1'):(Pure t2'):_)) = (t1 == t1') && (t2 == t2')
  f _ = False


-- Ad-hoc, from Altium export.  Recursive matching using sub,sub'
-- It's not clear if there are other patterns to support.  Fix when needed.
netlist edif = flat where
  grouped = map net nets
  flat = concat $ map (\(n, ps)  -> map (\ps -> (n,ps)) ps) grouped
  
  --             node  node_tag    node_name
  --------------------------------------------------
  [sheet] = sub' edif  "library"   "SHEET_LIB"
  [cell]  = sub  sheet "cell"
  [view]  = sub' cell  "view"      "netListView"
  [cont]  = sub  view  "contents"
  nets    = sub  cont  "Net"

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



-- HACKS

-- Alternative implementation using bottom scraper.  This is left here
-- for illustration.  Idea is nice but is very dirty in this case
-- because structure might not be so fixed.  Also, not very readable.
-- It's basically hard-to-read c[a|d]r programming. The use of
-- retract is just a notational short-hand.

netlist' :: EDIF -> [(String,(String,String))]
netlist' edif = map rel irefs' where

  -- Fish out a key that produces a single node as an anchor point to
  -- retrieve information..
  irefs' = irefs "InstanceRef" edif

  -- .. and fetch the rest from the based on relative coordinates.
  -- ("U8",[("InstanceRef",1),("PortRef",2),("Joined",1),("Net",2) ...
  --                  ("A23",[("PortRef",1),("Joined",1),("Net",2) ...
  --                                       ("SPI0_SCLK",[("Net",1) ...
  rel (1:2:j:2:p) = (net,(inst,port)) where
    inst = ref (1:2:j:2:p)
    port = ref   (1:j:2:p)
    net  = ref       (1:p)

  ref :: [Int] -> String
  ref = rename . retract . (refEdif edif) -- (1)

  -- Handle node rename forms.  Not sure how this works..
  rename [v] = v
  rename ["rename", n1, n2] = n2

instances edif = map rel irefs' where
  irefs' = irefs "Instance" edif
  ref = rename . retract . (refEdif edif) -- (1)
  rel (1:p) = (inst, typ) where
    inst = ref     (1:p)
    typ  = ref (1:2:2:p)
  rename [v] = v

-- Bottom scraper
irefs :: String -> EDIF -> [[Int]]
irefs name edif = map (map snd) $ Map.keys $ Map.filterWithKey f $ paths edif where
  f ((name',_):_) _ = name == name'
  f _ _ = False
  
-- (2) The use of coordinates is also very ad-hoc. It's basically
-- hard-to-read c[a|d]r programming, ignoring the node tags, and
-- assuming the structure is fixed.  Do this differently.
