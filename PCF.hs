module PCF where

--- ice40 PCF pin configuration files
pcf :: [String] -> (String -> String) -> String
pcf names pin = pcf' where
  pcf' = concat $ map set_io names
  set_io name =
    "set_io " ++
    name ++ " " ++
    (pin name) ++ "\n"

data PCF = PCF [String] (String->String)
instance Show PCF where
  show (PCF names pin) = pcf names pin
