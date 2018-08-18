-- Since we're just generating RTL, there is a fairly direct mapping
-- from SeqTerm to Verilog.

-- TODO: memories

module Verilog where

import Seq
import SeqTerm
import Data.List hiding (partition)
import Numeric (showHex, showIntAtBase)
import Data.Char

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
vModule mod_name portNames portTypes mod = Verilog portSpecs vCode where
  
  -- See SeqTerm for some post processing steps that are shared
  -- between HDLs.
  (portSpecs, (ports, bindings)) =
    SeqTerm.hdl_compile mod_name portNames portTypes mod

  -- Convert ports' bindings' to Verilog syntax

  part = partition' bindings

  comment t = " // " ++ show t
  -- comment _ = ""

  arrDecl (SInt (Just n) _) name sz =
    "reg [" ++ show (n-1) ++ ":0] " ++ name ++ "[0:" ++ show (sz-1) ++ "];"
  sigDecl kind (SInt (Just n) _) name =
    kind ++ " [" ++ show (n-1) ++ ":0] " ++ name ++ ";"
  -- sigDecl kind _ name = sigDecl kind (SInt (Just 123) 0) name -- FIXME
    
  decl typ b@(name, term) =
    (sigDecl typ) (termType term) name ++
    comment b ++ "\n"
  decls typ p = concat $ map (decl typ) $ part p

  mem_decls = concat $
    ((map memwr_decl $ part MemWrs) ++
     (map memrd_decl $ part MemRds))

  memrd_decl b@(name, (MemRd t (MemNode mem_name))) =
    -- Note: the read data register has a separate register name.
    sigDecl "reg" t name
    ++ comment b ++ "\n"
    
  memwr_decl b@(mem_name, (MemWr (we,wa,wd,ra))) =
    let arrSize = 2 ^ n
        (SInt (Just n) _) = opType wa
    in
      -- Note: the _ra signal is derived from the memory name.
      arrDecl (opType wd) mem_name arrSize ++ "\n" ++
      sigDecl "wire" (opType ra) (mem_name ++ "_ra") ++
      comment b ++ "\n"

  assigns = concat $ map assign $ (part Exprs ++ part Connects)
  assign b@(name, term) =
    "assign " ++ name ++ " = " ++ expr term ++ ";" ++
    comment b ++ "\n"

  updates = concat $ map update $ part Delays
  resets  = concat $ map reset  $ part Delays

  memwr_assigns = concat $ map memwr_assign $ part MemWrs
  memrd_updates = concat $ map memrd_update $ part MemRds
  memwr_updates = concat $ map memwr_update $ part MemWrs

  memrd_update b@(reg_name, (MemRd _ (MemNode mem_name))) =
    "always @(posedge CLK) begin\n" ++
    tab ++ reg_name ++ " <= " ++ mem_name ++ "[" ++ mem_name ++ "_ra];\n" ++
    "end\n"

  memwr_assign b@(mem_name, (MemWr (_,_,_,ra))) =
    "assign " ++ mem_name ++ "_ra = " ++ op ra ++ ";\n" ++
    comment b ++ "\n"
  
  memwr_update b@(mem_name, (MemWr (we,wa,wd,_))) =
    comment b ++ "\n" ++
    "always @(posedge CLK) begin\n" ++
    tab ++ "if (" ++ op we ++ ") begin\n" ++
    tab ++ tab ++ mem_name ++ "[" ++ op wa ++ "] <= " ++ op wd ++ ";\n" ++
    tab ++ "end\n" ++
    "end\n"


  reset b@(name, (Delay t _)) =
    tab ++ tab ++ name ++ " <= " ++ const t ++ ";" ++
    comment b ++ "\n"
  update b@(name, (Delay _ o)) =
    tab ++ tab ++ name ++ " <= " ++ op o ++ ";" ++
    comment b ++ "\n"

  op (Node _ name) = name
  op (Const t) = const t

  const (SInt Nothing v) = show v
  const (SInt (Just sz) v) = show sz ++ "'b" ++ bits where
    bits' = showIntAtBase 2 intToDigit v "" 
    bits  = replicate (sz - length bits') '0' ++ bits'

  expr (Comb1 _ INV o) = "!" ++ op o
  expr (Comb2 _ ADD a b) = op2 "+" [a,b]
  expr (Comb2 _ SUB a b) = op2 "-" [a,b]
  expr (Comb2 _ MUL a b) = op2 "*" [a,b]
  expr (Comb2 _ AND a b) = op2 "&" [a,b]
  expr (Comb2 _ OR  a b) = op2 "|" [a,b]
  expr (Comb2 _ XOR a b) = op2 "^" [a,b]
  expr (Comb2 _ SLL a b) = op2 "<<" [a,b]
  expr (Comb2 _ SLR a b) = op2 ">>" [a,b]
  expr (Comb2 _ CONC a b) = "{" ++ op a ++ ", " ++ op b ++ "}"
  expr (Comb2 _ EQU a b) = op2 "==" [a,b]
  expr (Comb3 _ IF a b c) = op a ++ " ? " ++ op b ++ " : " ++ op c
  expr (Connect _ o) = op o
  expr (Slice _ o (Just u) l) = op o ++ "[" ++ show (u-1) ++ ":" ++ show l ++ "]"
  expr e = "... /* " ++ show e ++ " */ "

  op2 opc [a,b] = op a ++ " " ++ opc ++ " " ++ op b

  commas = intercalate ", "
  -- portNames = map (\(Node _ name) -> name) ports

  tab = "    "
  
  vCode =
    -- "`timescale 1ns/10ps\n" ++  -- from MyHDL output
    "module " ++ mod_name ++ "(" ++ commas (["CLK","RST"] ++ portNames) ++ ");\n" ++
    "input CLK;\n" ++
    "input RST;\n" ++
    decls "wire"   Exprs ++
    decls "input"  Inputs ++
    decls "output" Connects ++
    decls "reg"    Delays ++
    mem_decls ++
    assigns ++
    memwr_assigns ++
    memrd_updates ++
    memwr_updates ++
    "always @(posedge CLK, negedge RST) begin: SEQ\n" ++
    tab ++ "if (RST==0) begin\n" ++
    resets ++
    tab ++ "end\n" ++
    tab ++ "else begin\n" ++
    updates ++
    tab ++ "end\n" ++
    "end\n" ++
    "endmodule\n"

-- reg [15:0] inst1_memory [0:4096-1];
-- wire [11:0] s55_wa;
-- reg [15:0] s55_rd;
-- wire [0:0] s55_we;
-- wire [15:0] s55_wd;
-- wire [11:0] s55_ra;
--
-- always @(posedge CLK) begin: F_SOC_INST1_RTLRD
--     s55_rd <= inst1_memory[s55_ra];
-- end
--
-- always @(posedge CLK) begin: F_SOC_INST1_RTLWR
--     if (s55_we) begin
--         inst1_memory[s55_wa] <= s55_wd;
--     end
-- end
