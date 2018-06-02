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

module EDIF(readEdifFile,readEdif,paths) where


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

-- Keep it really simple.  We'll do postprocessing, so no need to take
-- apart all the different node types.
type Leaf = String

-- With the only constraint on recursive nodes that there is at least
-- a tag node to make paths.
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




-- Convert tree to relation expressed as a list of unique paths.
-- Each "type" of path in the output set represents a different relation.

newtype PathsM t = PathsM { unPathsM :: StateT Paths (Reader UniquePath) t }
  deriving (Functor, Applicative, Monad,
            MonadState Paths, MonadReader UniquePath)

type Paths = Map UniquePath (Maybe Leaf)
type UniquePath = [(UniqueLeaf)]
type UniqueLeaf = (Leaf,Int)


paths :: EDIF -> Map UniquePath Leaf
paths edif = s' where
  -- To distinguish leaf from non-leaf intermediate path names, a Map
  -- to Bool is used.  When done, intermediate paths can be removed.
  s' = Map.mapMaybe id s
  ((), s) = runReader (runStateT (unPathsM $ iterPathsM edif) Map.empty) []

-- Explicit recursion.  Standard iterM doesn't fit the case here.  
iterPathsM :: EDIF -> PathsM ()
iterPathsM node = do
  here <- ask
  case node of
    (Pure val) -> do
      modify $ Map.insert here $ Just val
    (Free (Node (Pure tag) nodes)) -> do
      modify $ Map.insert here $ Nothing
      let sub node num = local (\_ -> here ++ [(tag, num)]) $ iterPathsM node
      sequence_ $ zipWith sub nodes [0..]

 
  




