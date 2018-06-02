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
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- Use Free to provide the nesting structure.  Use generic names
-- "Leaf" and "Node" since they are "the" leaf structure and tree
-- recursion structure.
type EDIF = Free Node Leaf

-- This allows focus on leaf nodes.  
data Leaf =
  -- Generic leaf node.
  L String | N Int
  -- EDIF-specific nodes
  -- | Edif
  -- | Library | Cell | View | Contents 
  -- | Instance | Property
  -- | Net | Joined | PortRef | InstanceRef
  -- | ViewRef | NetListView | CellRef
  -- | Interface | Port

  deriving (Show, Eq, Ord)

-- With the only constraint on recursive nodes that there is at least
-- a tag node to make paths.
data Node t = Node t [t] deriving Functor

-- Leaf constructor from Strings found by parser.
leaf :: String -> Leaf
--leaf ('&':str) = Ref $ read str  -- doesn't work yet
-- leaf "edif" = Edif
-- leaf "library" = Library
-- leaf "cell" = Cell
-- leaf "view" = View
-- leaf "contents" = Contents
-- leaf "Net" = Net
-- leaf "Joined" = Joined
-- leaf "PortRef" = PortRef
-- leaf "InstanceRef" = InstanceRef
-- leaf "Instance" = Instance
-- leaf "Property" = Property
-- leaf "viewRef" = ViewRef
-- leaf "cellRef" = CellRef
-- leaf "NetListView" = NetListView
-- leaf "interface" = Interface
-- leaf "port" = Port
-- Other, currently unused.
leaf str = L str


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
  return $ Pure $ L x
                
parseAtom :: Parser EDIF
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Pure $ leaf $ first:rest

parseNumber :: Parser EDIF
parseNumber = liftM (Pure . N . read) $ many1 digit               

parseExpr :: Parser EDIF
parseExpr  = spaces >> (parseAtom <|> parseNumber <|> parseString <|> parseList)
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

showLeafs as = concat $ map showLeaf as
showLeaf (L str) = str ++ "/"
-- showLeaf a = show a

showNode :: Node (ShowM Leaf) -> ShowM Leaf
showNode = down line




-- Convert tree to relation expressed as a list of unique paths.
-- Each "type" of path in the output set represents a different relation.

newtype PathMapM t = PathMapM { unPathMapM :: StateT PathMap (Reader UniquePath) t }
  deriving (Functor, Applicative, Monad,
            MonadState PathMap, MonadReader UniquePath)

type PathMap = Map UniquePath Bool
type UniquePath = [(UniqueLeaf)]
type UniqueLeaf = (Leaf,Int)


rels :: EDIF -> Set UniquePath
rels edif = s' where
  -- To distinguish leaf from non-leaf intermediate path names, a Map
  -- to Bool is used.  When done, intermediate paths can be removed.
  s' = Map.keysSet $ Map.filter id s
  ((), s) = runReader (runStateT (unPathMapM $ iterPathMapM edif) Map.empty) []


-- Explicit recursion.  Standard iterM doesn't fit the case here.  
iterPathMapM :: EDIF -> PathMapM ()
iterPathMapM node = do
  parent <- ask
  dict   <- get
  -- The need for this translation here probably indicates there is a
  -- better way to represent the data.
  let (name, isVal, children) = case node of
        (Pure val) -> (val, True, [])
        (Free (Node (Pure tag) nodes)) -> (tag, False, nodes)
        (Free (Node _ _)) -> error "Internal error: impure tag"
      paths = Map.keysSet dict
      subPath = unique paths parent name
  modify $ Map.insert subPath isVal
  local (\_ -> subPath) $ sequence_ $ map iterPathMapM children

  

-- Given current set, create a unique path from unique parent and
-- possibly non-unique leaf.
unique :: Set UniquePath -> UniquePath -> Leaf -> UniquePath
unique paths parent name = parent ++ [(name,n+1)] where
  n :: Int
  n = Set.foldr max' 0 paths where
  depth = length parent
  max' :: UniquePath -> Int -> Int
  max' p n = case parent == take depth p of
    True -> case drop depth p of
      [(name',i)] -> case name == name' of
        True -> max i n
        _ -> n
      _ -> n
    _ -> n


-- Next iteration: produce a map.  This means distinguishing leaf
-- nodes from path nodes.

