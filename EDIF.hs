-- EDIF netlist parser

-- EDIF is s-expression based.  Racket's default reader could not deal
-- with some special characters, so I'm just doing everything in
-- Haskell.  This is set up to evolve.  Currently only supports what's
-- needed for application.

-- LICENSE: This is derived from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
-- FIXME: currently assuming that's ok

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

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

-- Use Free to provide the nesting structure.  Use generic names
-- "Leaf" and "Node" since they are "the" leaf structure and tree
-- recursion structure.
type EDIF = Free Node Leaf

-- This allows focus on leaf nodes.  
data Leaf =
  -- EDIF-specific nodes
  Edif
  | Library | Cell | View | Contents 
  | Instance | Property
  | Net | Joined | PortRef | InstanceRef
  | ViewRef | NetListView | CellRef
  | Interface | Port
  -- Generic nodes
  | Atom String | Number Integer | String String

  deriving (Show, Eq, Ord)

-- With the only constraint on recursive nodes that there is at least
-- a tag node to make paths.
data Node t = Node t [t] deriving Functor

-- Leaf constructor from Strings found by parser.
leaf :: String -> Leaf
--leaf ('&':str) = Ref $ read str  -- doesn't work yet
leaf "edif" = Edif
leaf "library" = Library
leaf "cell" = Cell
leaf "view" = View
leaf "contents" = Contents
leaf "Net" = Net
leaf "Joined" = Joined
leaf "PortRef" = PortRef
leaf "InstanceRef" = InstanceRef
leaf "Instance" = Instance
leaf "Property" = Property
leaf "viewRef" = ViewRef
leaf "cellRef" = CellRef
leaf "NetListView" = NetListView
leaf "interface" = Interface
leaf "port" = Port
-- Other, currently unused.
leaf str = Atom str


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
  return $ Pure $ String x
                
parseAtom :: Parser EDIF
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Pure $ leaf $ first:rest

parseNumber :: Parser EDIF
parseNumber = liftM (Pure . Number . read) $ many1 digit               
                         
parseExpr :: Parser EDIF
parseExpr  = spaces >> (parseAtom <|> parseString <|> parseNumber <|> parseList)
parseList  = do
  char '(' ;
  tag <- parseExpr
  args <- sepEndBy parseExpr spaces1
  spaces ; char ')'
  return $ Free $ Node tag args


readEDIF :: String -> String -> Either String EDIF
readEDIF fileName contents =
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
readFile fileName = do
  h <- openFile fileName ReadMode
  hSetEncoding h latin1
  hGetContents h



-- How to work with a tree represented as a Free monad?
-- I've settled on iterM using a writer monad, and a reader containing the path.


-- List all nets based on a filter on path.
newtype PathM w t = PathM { unPathM :: WriterT w (Reader [Leaf]) t }
  deriving (Functor, Applicative, Monad,
            MonadReader [Leaf], MonadWriter w)

runPathM m = w where
  (_,w) = runReader (runWriterT (unPathM $ m)) []

-- FIXME: Abstract this further.
down mf (Node tag nodes) = do
  tag' <- tag
  mf tag'
  withPath tag' $ sequence_ [n >>= mf | n <- nodes]
  return tag'

withPath tag' m = do
  local (++ [tag']) m
  


-- Prettyprinter as a special case.
type ShowM = PathM String
show' :: EDIF -> String
show' edif = runPathM $ iterM showNode $ edif

tabs :: Int -> ShowM ()
tabs n = sequence_ $ [tell "  " | _ <- [1..n]]

line :: Leaf -> ShowM ()
line node = do
  path <- ask
  tabs $ length path
  tell $ showLeaf node ++ "\n"

-- line node = do
--   path <- ask
--   tell $ showLeafs path ++ showLeaf node ++ "\n"

showLeafs as = concat $ map showLeaf as
showLeaf (Atom str) = str ++ "/"
showLeaf a = show a

showNode :: Node (ShowM Leaf) -> ShowM Leaf
showNode = down line

-- Collect paths under a subpath.
type TableM = PathM[[Leaf]]
paths :: [Leaf] -> EDIF -> [[Leaf]]
paths parent edif = runPathM $ iterM (mPaths parent) edif

mPaths :: [Leaf] -> Node (TableM Leaf) -> TableM Leaf
mPaths parent nodes = down fm nodes where
  n = length parent
  fm tag = do
    path <- ask
    case parent == take n path of
      True -> tell $ [(drop n path) ++ [tag]]
      _ -> return ()

-- Note that in the EDIF case, paths like this are not unique.  To
-- make them unique, add some form of tagging.

-- Here's a different approach.  Transform the tree into a relation
-- based on paths.  Use a state monad instead of a writer.


  

-- Convert tree to relation expressed as a list of unique paths.  Each
-- "type" of path is a separate relation.

newtype RelM t = RelM { unRelM :: StateT PathRel (Reader UniquePath) t }
  deriving (Functor, Applicative, Monad,
            MonadState PathRel, MonadReader UniquePath)

type PathRel = Set UniquePath
type UniquePath = [(UniqueLeaf)]
type UniqueLeaf = (Leaf,Int)

runRelM s0 m = s where
  (_, s) = runReader (runStateT (unRelM $ m) s0) []

rels :: EDIF -> PathRel
rels edif = runRelM Set.empty $ iterRelM edif

-- Explicit recursion.  Standard iterM doesn't fit the case here.  
iterRelM :: EDIF -> RelM ()
iterRelM node = do
  -- The need for this translation here probably indicates there is a
  -- better way to represent the data structure.
  let (name, children) = case node of
        (Pure val) -> (val, [])
        (Free (Node (Pure tag) nodes)) -> (tag, nodes)
  parent <- ask
  paths  <- get
  let subPath = unique paths parent name
  modify $ Set.insert subPath
  local (\_ -> subPath) $ sequence_ $ map iterRelM children
  

-- Given current set, create a unique path from unique parent and
-- possibly non-unique leaf.
unique :: PathRel -> UniquePath -> Leaf -> UniquePath
unique paths parent name = parent ++ [(name,n+1)] where
  n :: Int
  n = Set.foldr max' 0 paths where
  depth = length parent
  max' :: UniquePath -> Int -> Int
  max' p n = case parent /= take depth p of
    True -> case drop depth p of
      [(name',i)] -> case name == name' of
        True -> max i n
        _ -> n
      _ -> n
    _ -> n


-- To create a flat node list, collecting just the paths has the
-- information spread out.  I need a way to:
-- 1) list all nodes under a subpath
-- 2) query the entire tree by going to the parent node

-- What does it mean to go to the parent node?  It's straightforward
-- to do with paths, as long as the paths are unique.  How to make
-- them unique?


-- Maybe uniqueness is not necessary if I can just "overwrite" the
-- last unique structure.  E.g. create something similar to a stack
-- trace.

-- [Net,Atom "SPI0_SCLK"]
-- [Net,Joined]
-- [Net,Joined,PortRef]
-- [Net,Joined,PortRef,Atom "A23"]
-- [Net,Joined,PortRef,InstanceRef]
-- [Net,Joined,PortRef,InstanceRef,Atom "U8"]
-- [Net,Joined,PortRef,InstanceRef]
-- [Net,Joined,PortRef]
-- [Net,Joined,PortRef]
-- [Net,Joined,PortRef,Atom "&16"]
-- [Net,Joined,PortRef,InstanceRef]
-- [Net,Joined,PortRef,InstanceRef,Atom "U9"]
-- [Net,Joined,PortRef,InstanceRef]
-- [Net,Joined,PortRef]
