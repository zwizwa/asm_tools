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

-- Use Free to provide the nesting structure in terms of ordinary
-- lists.  No need to get fancy.  Use generic names "Tree" and "Leaf"
-- since they are "the" tree and leaf.

type Tree = Free []
type EDIF = Tree Leaf

-- This allows focus on leaf nodes.  
data Leaf =
  -- Placeholders for information we don't use.
  Atom String | Number Integer | String String
  -- EDIF-specific nodes
  | Ref Integer
  | X
  deriving Show


-- Leaf constructor from Strings found by parser.
leaf :: String -> Leaf
--leaf ('&':str) = Ref $ read str  -- doesn't work yet
leaf str = Atom str


list' :: [EDIF] -> EDIF
list' = Free

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
parseList  = do char '(' ; x <- parseExprs ; spaces ; char ')' ; return x
parseExprs = liftM list' $ sepEndBy parseExpr spaces1

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
type M w t = WriterT w (Reader [Leaf]) t


down tag' mf nodes = local (++ [tag']) $ sequence_ [mf n | n <- nodes]

-- Prettyprinter is a special case:
newtype ShowM t = ShowM { runShowM :: M String t }
  deriving (Functor, Applicative, Monad, MonadReader [Leaf], MonadWriter String)
  
type IndentLevel = Int

show' :: EDIF -> String
show' edif = w where
  (_,w) = runReader (runWriterT $ runShowM $ iterM showNode $ edif) []

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

showNode :: [ShowM Leaf] -> ShowM Leaf
showNode (tag : nodes) = do
  tag' <- tag
  line tag'
  -- local (++ [tag']) $ sequence_ [do n' <- n ; line n' | n <- nodes]
  -- down tag' (\n -> do n' <- n ; line n') nodes
  down tag' (>>= line) nodes
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

 
-- List all nets based on a filter on path.
newtype TableM t = TableM { runTableM :: M String t }
  deriving (Functor, Applicative, Monad, MonadReader [Leaf], MonadWriter String)


table edif = w where
  (_,w) = runReader (runWriterT $ runTableM $ iterM filterJoined $ edif) []

filterJoined :: [TableM Leaf] -> TableM Leaf
filterJoined (tag : nodes) = do
  p <- ask
  tag' <- tag
  let p' = map (\(Atom tag) -> tag) p
  case p' of
    "edif":"library":"cell":"view":"contents":"Net":"Joined":"PortRef":sub ->
      tell $ show tag' ++ "\n"
                -- &6/
                -- InstanceRef/
                --   P3/
                -- InstanceRef/
    _ ->
      return ()
  down tag' (>>= (\_ -> return ())) nodes
  tag

-- tomorrow..
