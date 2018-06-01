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
import Control.Monad.Free
import System.IO

-- Use Free to provide the nesting structure.  Use generic names
-- "Leaf" and "Rec" since they are "the" leaf structure and tree
-- recursion structure.
type EDIF = Free Rec Leaf

-- This allows focus on leaf nodes.  
data Leaf =
  -- Placeholders for information we don't use.
  Atom String | Number Integer | String String
  -- EDIF-specific nodes
  | Ref Integer
  | X
  deriving Show

-- With the only constraint on recursive nodes that there is at least
-- a tag node to make paths.
data Rec t = Rec t [t] deriving Functor

-- Leaf constructor from Strings found by parser.
leaf :: String -> Leaf
--leaf ('&':str) = Ref $ read str  -- doesn't work yet
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
  return $ Free $ Rec tag args


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
  deriving (Functor, Applicative, Monad, MonadReader [Leaf], MonadWriter w)

runPathM m = w where
  (_,w) = runReader (runWriterT $ unPathM $ m) []

-- FIXME: Abstract this further.
down mf (Rec tag nodes) = do
  tag' <- tag
  mf tag'
  local (++ [tag']) $ sequence_ [n >>= mf | n <- nodes]
  return tag'

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

showNode :: Rec (ShowM Leaf) -> ShowM Leaf
showNode mNodes = do
  -- local (++ [tag']) $ sequence_ [do n' <- n ; line n' | n <- nodes]
  -- down tag' (\n -> do n' <- n ; line n') nodes
  tag' <- down line mNodes
  return tag'

-- EXAMPLE: cut off at view by not executing the monad components
-- descend :: [M Leaf] -> M Leaf
-- descend (tag : nodes) = do
--   tag' <- tag
--   line tag'
--   case tag' of
--     Atom "view" ->
--       -- Stop here
--       return ()
--     _ ->
--       local (++ [tag']) $ sequence_ [do n' <- n ; line n' | n <- nodes]
--   return tag'

 
type TableM = PathM String
table :: EDIF -> String
table edif = runPathM $ iterM filterJoined edif

filterJoined :: Rec (TableM Leaf) -> TableM Leaf
filterJoined nodes = down fm nodes where
  fm _ = do
    path <- ask
    let path' = map (\(Atom str) -> str) path
    case path' of
      "edif":"library":"cell":"view":"contents":"Net":"Joined":"PortRef":sub ->
        tell $ show path ++ "\n"
        -- &6/
        -- InstanceRef/
        --   P3/
        -- InstanceRef/
      _ ->
        return ()

-- tomorrow..

-- Parameterize the functor with a tag type that can be used to
-- construct paths as [tag].

