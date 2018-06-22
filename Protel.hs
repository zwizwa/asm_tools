-- Protel netlist parser

-- Ad hoc, Stringly-typed.
-- See also SE.hs

-- LICENSE: This is derived from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

{-# LANGUAGE FlexibleContexts #-}

module Protel where

import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Free
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- Protel netlist format contains two parts:
-- [] Component definitions
-- () Net definitions.

type Protel = ([Comp], [Net])
type Net  = (Name, [Pin])
type Pin  = (Name, Name)
type Comp = [Name]
type Name = String

crlf = do char '\r' ; char '\n'


line = do
  l <- many $ noneOf "[]()\n\r" ; crlf
  return l

pin :: Parser Pin
pin = do
  compName <- many $ noneOf "-[]()\n\r"
  char '-'
  pinName <- line
  return (compName, pinName)
  

net :: Parser Net
net = do
  char '(' ; crlf
  name <- line
  lines <- many pin
  char ')' ; crlf
  return (name, lines)
  

comp :: Parser Comp
comp = do
  char '[' ; crlf
  lines <- many line
  char ']' ; crlf
  return lines


parseFile :: Parser Protel
parseFile = do
  comps <- many comp
  nets  <- many net
  eof
  return (comps, nets)


readProtel :: String -> String -> Either String Protel
readProtel fileName contents =
  case parse parseFile "protel netlist" contents of
    Right parsed -> Right parsed
    Left parseError -> Left msg' where
      ep = errorPos parseError
      l = show $ sourceLine ep
      msg = show parseError
      msg' = fileName ++ ":" ++ l ++ ": " ++ msg
