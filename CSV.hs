-- Multi-table CSV parser.  Should survive round-trips to Excel/LibreOffice

module CSV where

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

type Field = String
type Record = [Field]
type Table = [Record]

-- Parser

-- FIXME: remove comments earlier

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

comment :: Parser Record
comment = do
  char '#'
  many $ noneOf "\n\r"
  return [""]

table :: Parser Table
table = do
  t <- sepBy (comment <|> record) newline
  -- eof
  return t


-- Cleanup.  This could go in the CSV parser, but it seems simpler to
-- allow non-rectangular data in CSV, and have a separate
-- table-checker. FIXME: Use Either instead of error.

checkTable table = (h, rs') where
  rs' = map checkRow rs
  (h:rs) = filter keep table
  n = length h
  checkRow r = case n == length r of
    True -> r
    False -> error $ "checkTable: bad row " ++ show (n, r)

  -- Skip comments and blank lines
  -- keep (('#':_):_) = False
  keep [""] = False -- empties are ok, just drop them
  keep _ = True

    
    




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


