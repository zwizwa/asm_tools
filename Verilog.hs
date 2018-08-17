-- Since we're just generating RTL, there is a fairly direct mapping
-- from SeqTerm to Verilog.

module Verilog where

import Seq
import SeqTerm


-- Some simplifications:
-- . All signals are vectors.
-- . Keep operations in ANF
-- . One block for registers, CLK, RST
-- . One block for memories, CLK only


sig kind (SInt (Just n) _) name = kind ++ " [" ++ show (n-1) ++ ":0] " ++ name ++ ";"
wire   = sig "wire"
reg    = sig "reg"
input  = sig "input"
output = sig "output"

type PortSpec = (String,Int)
data Verilog = Verilog [PortSpec] String
instance Show Verilog where
  show (Verilog specs code) = str where
    str = code ++ "\nports = " ++ show specs  ++ "\n"

  
vModule :: String -> [String] -> [SType] -> ([R S] -> M ()) -> Verilog
vModule name portNames portTypes mod = Verilog portSpecs' vCode where
  
  -- See SeqTerm for some post processing steps that are shared
  -- between HDLs.
  (portSpecs', (ports', bindings')) =
    SeqTerm.hdl_compile name portNames portTypes mod

  -- Convert ports' bindings' to Verilog syntax

  decls = concat $ map decl bindings'
  decl (name, term) = wire (termType term) name ++ "\n"
  
  vCode =
    "// name: " ++ name ++ "\n" ++
    "// ports' " ++ show ports' ++ "\n" ++
    "// bindings'" ++ show bindings' ++ "\n" ++
    decls
    

