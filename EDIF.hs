-- EDIF netlist parser

-- This is set up to evolve.
-- Currently only supports what's needed for application.

module EDIF where

-- EDIF is s-expression based.  Racket's default reader could not deal
-- with some special characters, so using a modification of:
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Free
import System.IO

-- Use Free to provide the nesting structure in terms of ordinary
-- lists.  No need to get fancy.
type EDIF = Free [] Leaf

-- This allows focus on leaf nodes.  
data Leaf =
   -- Here, we have some generic placeholders for information we don't
   -- need yet.
  Atom String
  | Number Integer
  | String String
  deriving Show

-- Leaf constructor from Strings found by parser.
leaf :: String -> Leaf
leaf = Atom


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
                         
parseList :: Parser EDIF
parseList = liftM list' $ sepEndBy parseExpr spaces1

parseQuoted :: Parser EDIF
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ list' [Pure $ Atom "quote", x]

-- This is where leading spaces are consumed.  Trailing spaces can be
-- left.
parseExpr :: Parser EDIF
parseExpr = spaces >> parseExpr'
parseExpr' =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do char '(' ; x <- parseList
         spaces ; char ')'
         return x

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
