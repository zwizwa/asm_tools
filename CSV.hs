-- Multi-table CSV parser.  Should survive round-trips to Excel/LibreOffice

module CSV where

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

type Field = String
type Record = [Field]
type Table = [Record]

-- Parser

quoted :: Parser Field
quoted = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return x
                
unquoted :: Parser Field
unquoted = many $ noneOf ",\"\n\r"

field :: Parser Field
field =  quoted <|> unquoted

record :: Parser Record
record  = sepBy field $ char ','

table :: Parser Table
table = sepBy record newline


readCSV :: String -> String -> Either String Table
readCSV fileName contents =
  case parse table "table" contents of
    Right parsed -> Right parsed
    Left parseError -> Left msg' where
      ep = errorPos parseError
      l = show $ sourceLine ep
      -- c = show $ sourceColumn ep
      -- msg = mconcat $ fmap messageString $ errorMessages parseError
      msg = show parseError
      msg' = fileName ++ ":" ++ l ++ ": " ++ msg


