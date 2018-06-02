-- EDIF netlist parser

-- Evolved into generic SExp -> UniquePath -> Leaf

-- LICENSE: This is derived from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
-- FIXME: currently assuming that's ok

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module EDIF(readEdifFile,readEdif,paths,libraries,nodeName) where


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

-- Use Free to provide the nesting structure.  Use generic names
-- "Leaf" and "Node" since they are "the" leaf structure and tree
-- recursion structure in this module.
type EDIF = Free Node Leaf

-- Keep it really simple.  We'll do postprocessing, so no need to take
-- apart all the different node types.  We're primarily interested in
-- just getting at the data.
type Leaf = String

-- Nodes do need a tag to make (human readable) paths, so they have a
-- little bit more structure than ordinary lists.
data Node t = Node t [t] deriving Functor

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
  return $ Free $ Node tag args


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

paths edif = s where
  ((), s) = runReader (runStateT (unPathsM $ iterPathsM edif) Map.empty) []

-- Explicit recursion.  Standard iterM doesn't fit the case here.  
iterPathsM :: EDIF -> PathsM ()
iterPathsM node = do
  here <- ask
  case node of
    (Pure val) -> do
      modify $ Map.insert here $ val
    (Free (Node (Pure tag) nodes)) -> do
      let sub node num = local (\_ -> here ++ [(tag, num)]) $ iterPathsM node
      sequence_ $ zipWith sub nodes [0..]
    (Free (Node _  nodes)) -> error "impure node tag" -- doesn't happen



-- Queries.
  

-- Note: while Free is used for the data structure, none of its
-- properties are actually used in parsing nor path construction.

-- Next: map the PathToLeaf dictionary to some simpler functions.  
-- Each "type" of path in the output set represents a different relation.

-- What I want: a simple list of netlists: [(name,[(instance,pin)])]

netlist :: EDIF -> [(String,[(String,String)])]
netlist _ = []

-- Parent : ([("edif",6),("library",3),("cell",2),("view",3),("contents",91)
-- Sub:
-- ("Net",0)],"SPI0_SCLK")
-- ("Net",1),("Joined",0),("PortRef",0)],"A23")
-- ("Net",1),("Joined",0),("PortRef",1),("InstanceRef",0)],"U8")
-- ("Net",1),("Joined",1),("PortRef",0)],"&16")
-- ("Net",1),("Joined",1),("PortRef",1),("InstanceRef",0)],"U9")

-- How to do this properly?
-- Let's do it in a dirty way first:
-- Find all like this: ("Net",1),("Joined",0),("PortRef",1),("InstanceRef",0)],"U8")
-- Then from the path, get the portref and netname.

-- EDIT: Rethinking.  I don't really need paths for referencing.  I
-- only needed paths for prettyprinting.  Once the structure is known,
-- referencing subtrees should do it.

-- Some basic operations: filter nodes by tag.


-- First atom is always type.  Second atom is sometimes name, but not
-- always.  This is enough for what is needed below.
nodeName n = t where (Pure t) = nodeRef 0 n


nodeRef i (Free (Node _ ns)) = ns !! i

libraries (Free (Node (Pure "edif") nodes)) =
  filterTags "library" nodes

filterTags :: String -> [EDIF] -> [EDIF]
filterTags tag = filter f where
  f (Free (Node (Pure t) _)) = t == tag
  f _ = False


-- Finding things.  I find it simplest to think in terms of
-- subdirectories and going to parent to find things.  This can be
-- done using paths.




