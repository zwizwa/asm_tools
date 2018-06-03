-- EDIF netlist parser

-- Evolved into generic SExp -> UniquePath -> Leaf

-- LICENSE: This is derived from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
-- FIXME: currently assuming that's ok

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Data.Foldable
import GHC.Generics

-- Use Free to provide the nesting structure.  Use generic names
-- "Leaf" and "Node" since they are "the" leaf structure and tree
-- recursion structure in this module.
type EDIF = Free Node Leaf

-- Keep it really simple.  We'll do postprocessing, so no need to take
-- apart all the different node types.  We're primarily interested in
-- just getting at the data.
type Leaf = String

-- Overall it seems simplest to just wrap a list.
newtype Node t = Node [t] deriving (Functor, Applicative, Monad)


-- Addresses are backwards paths by default.
refEdif n is = refEdif' n (reverse is)
refEdif' = f where
  f n [] = n
  f (Free (Node nodes)) (i:is) = f (nodes !! i) is



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
  return $ Free $ Node (tag:args)


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
-- original file structure.  Type information is tagged on to keep
-- paths human-readable, and allow semantic filter operations without
-- a need for data structure lookups.
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
    (Free (Node ((Pure tag):nodes))) -> do
      let sub node num = local (\_ -> (tag,num):here) $ iterPathsM node
      sequence_ $ zipWith sub nodes [1..]
    (Free (Node _)) -> error "impure node tag" -- doesn't happen



-- Note: while Free is used for the data structure, none of its
-- properties are actually used in parsing nor path construction.




-- Queries.

-- There are basically two ways to approach this:

-- a) Top-down: match / query starting at the top of the hierarchy
-- b) Bottom-up: filter nodes from zipper paths

-- The former is straightforward but tedious to work through the
-- boilerplate.  If structure permits, it's possible to take a
-- bottom-up approach by zooming in on a particular collection of
-- nodes and operating on its context using relative paths.  The
-- latter is a lot less typing.

netlist :: EDIF -> [(String,String,String)]
netlist edif = map rel irefs where

  -- Fish out a key that produces a single node for each relation we
  -- want to return.
  irefs :: [[Int]]
  irefs = map (map snd) $ Map.keys $ Map.filterWithKey f $ paths edif
  f (("PortRef",_):_) _ = True
  f _ _ = False

  -- Then fetch all information from that node's location, assuming
  -- the structure is rigid such that coordinates can be used. E.g.:
  -- ("U8",[("InstanceRef",0),("PortRef",1),("Joined",0),("Net",1) ...
  --                  ("A23",[("PortRef",0),("Joined",0),("Net",1) ...
  --                                       ("SPI0_SCLK",[("Net",0) ...
  rel (1:j:2:up) = (name,inst,port) where
    inst = ref (1:2:j:2:up)
    port = ref   (1:j:2:up)
    name = ref       (1:up)

  ref :: [Int] -> String
  ref = rename . (refEdif edif)

  -- Handle node rename forms.  Not sure how this works..
  rename (Pure v) = v
  rename (Free (Node [Pure "rename", Pure n1, Pure n2])) = n2



-- TODO: Is this just because I can, or is it actually useful?
 
    


