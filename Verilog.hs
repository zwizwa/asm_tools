-- Since we're just generating RTL, there is a fairly direct mapping
-- from SeqTerm to Verilog.

module Verilog where

import Seq
import SeqTerm
import Data.List hiding (partition)


-- Some simplifications:
-- . All signals are vectors.
-- . Keep operations in ANF
-- . One block for registers, CLK, RST
-- . One block for memories, CLK only


type PortSpec = (String,Int)
data Verilog = Verilog [PortSpec] String
instance Show Verilog where
  show (Verilog specs code) = str where
    str = code

-- Several targets need different partitioning, so copy paste and edit.
data Part = Connects | Delays | Inputs | MemRds | MemWrs | Exprs deriving Eq
partition' bindings t = map snd $ filter ((t ==) . fst) tagged where
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)     = Inputs
  p (_, Connect _ _) = Connects
  p (_, Delay _ _)   = Delays
  p (_, MemRd _ _)   = MemRds
  p (_, MemWr _)     = MemWrs
  p _                = Exprs

  
vModule :: String -> [String] -> [SType] -> ([R S] -> M ()) -> Verilog
vModule name portNames portTypes mod = Verilog portSpecs vCode where
  
  -- See SeqTerm for some post processing steps that are shared
  -- between HDLs.
  (portSpecs, (ports, bindings)) =
    SeqTerm.hdl_compile name portNames portTypes mod

  -- Convert ports' bindings' to Verilog syntax

  part = partition' bindings

  --comment t = " // " ++ show t
  comment _ = ""

  sigDecl kind (SInt (Just n) _) name =
    kind ++ " [" ++ show (n-1) ++ ":0] " ++ name ++ ";"
  decl typ b@(name, term) =
    (sigDecl typ) (termType term) name ++
    comment b ++ "\n"
  decls typ p = concat $ map (decl typ) $ part p

  assigns = concat $ map assign $ (part Exprs ++ part Connects)
  assign b@(name, term) =
    "assign " ++ name ++ " = " ++ expr term ++ ";" ++
    comment b ++ "\n"

  updates = concat $ map update $ part Delays
  resets  = concat $ map reset  $ part Delays

  reset b@(name, (Delay (SInt _ rv) _)) =
    tab ++ tab ++ name ++ " <= " ++ show rv ++ ";" ++
    comment b ++ "\n"
  update b@(name, (Delay _ o)) =
    tab ++ tab ++ name ++ " <= " ++ op o ++ ";" ++
    comment b ++ "\n"

  expr (Comb1 _ INV o) = "!" ++ op o
  expr (Connect _ o) = op o
  expr _ = "..."

  op (Node _ name) = name
  op (Const (SInt _ val)) = show val

  commas = intercalate ", "
  -- portNames = map (\(Node _ name) -> name) ports

  tab = "    "
  
  vCode =
    -- "`timescale 1ns/10ps\n" ++  -- from MyHDL output
    "module " ++ name ++ "(" ++ commas (["CLK","RST"] ++ portNames) ++ ");\n" ++
    "input CLK;\n" ++
    "input RST;\n" ++
    decls "wire"   Exprs ++
    decls "input"  Inputs ++
    decls "output" Connects ++
    decls "reg"    Delays ++
    assigns ++
    "always @(posedge CLK, negedge RST) begin: SEQ\n" ++
    tab ++ "if (RST==0) begin\n" ++
    resets ++
    tab ++ "end\n" ++
    tab ++ "else begin\n" ++
    updates ++
    tab ++ "end\n" ++
    "end\n" ++
    "endmodule\n"
    

