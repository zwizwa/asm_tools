-- (DESC) S-expression parser. Ad hoc.

-- Ad hoc, Stringly-typed.

-- TODO: This contains some leftovers ideas on how to approach data
-- exctraction from large ad-hoc tree structures.  See HACKS below.

-- LICENSE: This is derived from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

-- Mod 2020/2/6: Make it work for PcbNew format.  This probably
-- bitrotted old code.  Find a better way to parameterize this.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.AsmTools.SE where

import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Free
import Data.Map.Strict(Map)
import Data.Maybe
import qualified Data.Map.Strict as Map

-- Keep it really simple.
type Leaf = String
type Node = ([])
type SE = Free Node Leaf


-- Parser

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseString :: Parser SE
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  -- Not necessary to distinguish from Atom
  return $ Pure $ x
                
parseAtom :: Parser SE
parseAtom = do 
  str <- many1 (letter <|> digit <|> symbol)
  return $ Pure $ str

parseExpr :: Parser SE
parseExpr  = spaces >> (parseAtom <|> parseString <|> parseList)
parseList  = do
  char '(' ;
  tag <- parseExpr
  args <- sepEndBy parseExpr spaces1
  spaces ; char ')'
  return $ Free (tag:args)


readSE :: String -> String -> Either String SE
readSE fileName contents =
  case parse parseExpr "lisp" contents of
    Right parsed -> Right parsed
    Left parseError -> Left msg' where
      ep = errorPos parseError
      l = show $ sourceLine ep
      -- c = show $ sourceColumn ep
      -- msg = mconcat $ fmap messageString $ errorMessages parseError
      msg = show parseError
      msg' = fileName ++ ":" ++ l ++ ": " ++ msg


-- Addresses are backwards paths by default.
refSE n is = refSE' n (reverse is)
refSE' = f where
  f n [] = n
  f (Free nodes) (i:is) = f (nodes !! i) is



-- Given node, find all subnodes with a particular node type tag.
sub :: SE -> Leaf -> [SE]
sub (Free (_:ns)) t = filter f ns where
  f (Free ((Pure t'):_)) = t == t'
  f _ = False

-- Same, but for the 2-tag pattern: node type, node name
sub' :: SE -> Leaf -> Leaf -> [SE]
sub' (Free (_:ns)) t1 t2 = filter f ns where
  f (Free ((Pure t1'):(Pure t2'):_)) = (t1 == t1') && (t2 == t2')
  f _ = False




-- Path representation of SE tree.

-- Convert parser tree output to path->leaf map
paths :: SE -> PathToLeaf

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
iterPathsM :: SE -> PathsM ()
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




-- Parsing by matching is actually quite a pain because there are so
-- many cases that are not part of the grammar.  That is obvious in
-- retrospect but was an unexpected complication for me in general,
-- when mapping "dynamic" data types into a statically typed language.

-- It is as if there really isn't any reason to split this up.
-- E.g. don't parse to s-expressions, but parse to the language's
-- expression syntax instead.

type M = Either String

string :: SE -> M String
string (Pure a) = Right a
string e = Left $ "expected (Pure a), got " ++ show e

list :: SE -> M [SE]
list (Free l) = Right l
list e = Left $ "expected (Free l), got " ++ show e

-- Assert that it is a tagged list, but tag is allowed to fail
untag :: SE -> M (String, [SE])
untag expr = do
  l <- list expr
  case l of
    (Pure tag : l') ->
      Right (tag, l')
    _ ->
      Left $ "expected tagged list, got: " ++ show expr

tagged tag expr = do
  (tag', l) <- untag expr
  if tag == tag' then Right l
    else Left $ "expected tag: " ++ tag ++ ", got tag: " ++ tag'
  

filter_tag :: String -> [SE] -> M [SE]
filter_tag tag exprs = do
  let fltr expr = do
        (t, l) <- untag expr
        return $ case t == tag of
                   True  -> Just expr
                   False -> Nothing
  mls <- traverse fltr exprs
  return $ catMaybes mls
