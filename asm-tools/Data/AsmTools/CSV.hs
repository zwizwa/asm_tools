-- Multi-table CSV parser.  Should survive round-trips to Excel/LibreOffice

-- TODO: add some TH to avoid IO.

module Data.AsmTools.CSV where

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Data.List

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

newline' = (char '\r' >> char '\n') <|> char '\n'

table :: Parser Table
table = do
  t <- sepBy (comment <|> record) newline'
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

readCSVFile :: [Char] -> IO ([String],[[String]])
readCSVFile file = do
  str <- readFile file
  case fmap checkTable $ readCSV file str of
    Right table -> return table
    Left msg -> error msg

readTagged tag file = do
  (header, table) <- readCSVFile file
  return $ map tag table
  
-- Finite function evaluator.
ff :: (Show k, Eq k, Show v) => (r -> (k,v)) -> [r] -> k -> v
ff kv table = flip lookup' $ map kv table

lookup' k l = case lookup k l of
  Just v -> v
  Nothing -> error $ "lookup': " ++ show (k,l)


-- Convert parsed form back to text

showCSVFile :: ([String],[[String]]) -> String
showCSVFile (header, rows) = line header ++ (concat $ map line rows) where
  line cols = (intercalate "," cols) ++ "\n"

-- writeCSVFile file table = writeFile file $ showCSVFile table


-- Or a SQL
showSQL :: String -> ([String],[[String]]) -> String
showSQL name (header, rows) = sql where
  sql = "begin transaction;\n" ++
        create ++ concat (map insert rows) ++
        "end transaction;\n"
  create = "create table " ++ name ++ "(" ++ commas header ++ ");\n"
  insert row = "insert into " ++ name ++ " values(" ++ commas (map show row) ++ ");\n"
  commas = intercalate ", "
