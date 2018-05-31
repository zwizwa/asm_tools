-- EDIF netlist parser

module EDIF where

-- EDIF is s-expression based.  Racket's default reader could not deal
-- with some special characters, so using a modification of:
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Free

data Leaf = Atom   String
          | Number Integer
          | String String
          | Bool   Bool
  deriving Show

type EDIF = Free [] Leaf

list' :: [EDIF] -> EDIF
list' = Free


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
              let atom = first:rest
              return $ Pure $
                case atom of 
                  "#t" -> Bool True
                  "#f" -> Bool False
                  _    -> Atom atom

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


