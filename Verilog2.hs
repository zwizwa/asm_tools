-- FIXME: express this first in terms of
-- [(Vertex, (SSize, Expr Vertex))]



-- Since we're just generating RTL, there is a fairly direct mapping
-- from SeqTerm to Verilog.
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Verilog2 where

import Seq(S,SType(..),Op1(..),Op2(..),Op3(..))
import qualified SeqTerm
import SeqNetList
import Data.List hiding (partition)
import Numeric (showHex, showIntAtBase)
import Data.Char
import qualified SeqLib
import Data.Bits
import Control.Monad.Free
import Data.Graph

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
data Part = Connects | Delays | Inputs | Memories | Exprs deriving Eq

partition' bindings t = map snd $ filter ((t ==) . fst) tagged where
  tagged = map p' bindings
  p' x@(n, (t, (Free form))) = (p form, x)
  p' x = error $ "partition': improper inlined form"
  p (Input)          = Inputs
  p (Connect _)      = Connects
  p (Delay _ _)      = Delays
  p (Memory _ _ _ _) = Memories
  p _                = Exprs

  
vModule :: String -> [String] -> [SType] -> ([SeqTerm.R S] -> SeqTerm.M ()) -> Verilog
vModule mod_name portNames portTypes mod = Verilog portSpecs vCode where
  
  -- See SeqTerm for some post processing steps that are shared
  -- between HDLs.

  mod' = do
    -- Ports default as input.
    -- When assigned through 'update', type changes from in->out
    io <- SeqTerm.inputs $ portTypes
    mod io ; return io

  -- Graph algos only operate on Vertex
  (ports, bindings', probes) = SeqTerm.compileTerm mod'
  netlist@(NetList _ bindmap) = convert ports bindings'
  dag = toDAG bindmap
  -- Convert node rep to String
  bindings = map nameBinding $ inlined dag where
    -- FIXME: use probe names
    nameBinding (n,(t,e)) = (nameNode n,(t,fmap nameNode e))
    nameNode = ("s" ++) . show
  
  part = partition' bindings

  portSpecs = undefined
  
  --(portSpecs, (ports, bindings')) =
  --  SeqTerm.hdl_compile' portNames portTypes mod


  -- Convert ports' bindings' to Verilog syntax

  decls typ p = concat $ map (decl typ) $ part p

  -- mem_decls = concat $ map memory_decl $ part Memories

  -- assigns = concat $ map assign $ (part Exprs ++ part Connects)

  updates = concat $ map update $ part Delays
  resets  = concat $ map reset  $ part Delays

  -- memwr_assigns = concat $ map memwr_assign $ part MemWrs
  -- memrd_updates = concat $ map memrd_update $ part MemRds
  -- memwr_updates = concat $ map memwr_update $ part MemWrs

  vCode =
    -- "`timescale 1ns/10ps\n" ++  -- from MyHDL output
    "module " ++ mod_name ++ "(" ++ commas (["CLK","RST"] ++ portNames) ++ ");\n" ++
    "input CLK;\n" ++
    "input RST;\n" ++
    decls "input"  Inputs ++
    decls "output" Connects ++
    decls "wire"   Exprs ++
    decls "reg"    Delays ++
    "always @(posedge CLK, negedge RST) begin: SEQ\n" ++
    tab ++ "if (RST==0) begin\n" ++
    resets ++
    tab ++ "end\n" ++
    tab ++ "else begin\n" ++
    updates ++
    tab ++ "end\n" ++
    "end\n" ++
    "endmodule\n"
    
  -- vCode =
  --   mem_decls ++
  --   assigns ++
  --   memwr_assigns ++
  --   memrd_updates ++
  --   memwr_updates ++


tab = "    "
debug t = " // " ++ show t ++ "\n"
-- debug _ = "//\n"

-- arrDecl (SInt (Just n) _) name sz =
--   "reg [" ++ show (n-1) ++ ":0] " ++ name ++ "[0:" ++ show (sz-1) ++ "];"
-- arrDecl _ name _ = error $ "arrDecl needs fixed bit size: " ++ name

sigDecl kind (Just n) name =
  kind ++ " [" ++ show (n-1) ++ ":0] " ++ name ++ ";"

decl typ b@(name, (bits, term)) =
  (sigDecl typ) bits name ++ debug b

-- ---- FIXME: Proper unification on SeqTerm data structure is needed to
-- ---- determine all node types.  Example:
-- ---- // "s144" <- (IF "rx_in" (CONST _:0) (CONST _:1)) As a workaround,
-- ---- I thought about defaulting to a fixed size and leave it for yosys
-- ---- to optimize.  This doesn't work for concat.
-- -- sigDecl kind _ name = hackDecl ++ msg where 
-- --   hackDecl = sigDecl kind (SInt (Just 32) 0) name
-- --   msg = " // unknown size: " ++ name
-- sigDecl _ _ name = error $ "sigDecl needs fixed bit size: " ++ name


proj_err b = error $ "projection error: " ++ show b

-- memrd_decl b@(name, (MemRd t (MemNode mem_name))) =
--   -- Note: the read data register has a separate register name.
-- memrd_decl b = proj_err b


-- memory_decl @(mem_name, (MemWr (we,wa,wd,ra))) =
--   let arrSize = 2 ^ n
--       (SInt (Just n) _) = opType wa
--   in
--     -- Note: the _ra signal is derived from the memory name.
--     sigDecl "reg" t name ++ debug b
--     arrDecl (opType wd) mem_name arrSize ++
--     debug b ++
--     sigDecl "wire" (opType ra) (mem_name ++ "_ra") ++
--     debug b
-- memwr_decl b = proj_err b


-- memrd_update b@(reg_name, (MemRd _ (MemNode mem_name))) =
--   "always @(posedge CLK) begin\n" ++
--   tab ++ reg_name ++ " <= " ++
--   mem_name ++ "[" ++ mem_name ++ "_ra];" ++ debug b ++
--   "end\n"
-- memrd_update b = proj_err b


-- memwr_assign b@(mem_name, (MemWr (_,_,_,ra))) =
--   "assign " ++ mem_name ++ "_ra = " ++ op ra ++ ";" ++
--   debug b
-- memwr_assign b = proj_err b
  
-- memwr_update b@(mem_name, (MemWr (we,wa,wd,_))) =
--   "always @(posedge CLK) begin\n" ++
--   tab ++ "if (" ++ op we ++ ") begin\n" ++
--   tab ++ tab ++ mem_name ++ "[" ++ op wa ++ "] <= " ++
--   op wd ++ ";" ++ debug b ++
--   tab ++ "end\n" ++
--   "end\n"
-- memwr_update b = proj_err b

-- assign b@(name, term) =
--   "assign " ++ name ++ " = " ++ expr term ++ ";" ++
--   debug b

reset b@(name, (t, (Free (Delay _ i)))) =
  tab ++ tab ++ name ++ " <= " ++ literal (t,i) ++ ";" ++
  debug b
reset b = proj_err b

update b@(name, (_, (Free (Delay (Pure n) _)))) =
  tab ++ tab ++ name ++ " <= " ++ n ++ ";" ++
  debug b
update b = proj_err b
  
literal (Nothing, v) = show v
literal ((Just sz), v) = show sz ++ "'b" ++ bits where
  v' = v .&. ((1 `shiftL` sz)-1)
  bits' = showIntAtBase 2 intToDigit v' "" 
  bits  = replicate (sz - length bits') '0' ++ bits'

-- op (Node _ name) = name
-- op (Const t) = literal t
-- op (MemNode name) = error $ "op: MemNode projection error: " ++ name

-- expr (Comb1 _ INV o) = "!" ++ op o
-- expr (Comb2 _ ADD a b) = op2 "+" a b
-- expr (Comb2 _ SUB a b) = op2 "-" a b
-- expr (Comb2 _ MUL a b) = op2 "*" a b
-- expr (Comb2 _ AND a b) = op2 "&" a b
-- expr (Comb2 _ OR  a b) = op2 "|" a b
-- expr (Comb2 _ XOR a b) = op2 "^" a b
-- expr (Comb2 _ SLL a b) = op2 "<<" a b
-- expr (Comb2 _ SLR a b) = op2 ">>" a b
-- expr (Comb2 _ CONC a b) = "{" ++ op a ++ ", " ++ op b ++ "}"
-- expr (Comb2 _ EQU a b) = op2 "==" a b
-- expr (Comb3 _ IF a b c) = op a ++ " ? " ++ op b ++ " : " ++ op c
-- expr (Connect _ o) = op o
-- expr (Slice _ o (Just u) l) = op o ++ "[" ++ show (u-1) ++ ":" ++ show l ++ "]"
-- expr e = "... /* " ++ show e ++ " */ "

-- op2 opc a b = op a ++ " " ++ opc ++ " " ++ op b

commas = intercalate ", "

-- -- Catch some degenerate cases.
-- fix_binding b@(name, (MemWr _)) = b
-- fix_binding b@(name, term) =
--   case termType term of
--     (SInt Nothing _) ->
--       -- error $ "fix_binding: " ++ show b
--       b
--     _ ->
--       b

--------------------------------------------------------------------------------------


-- fpgaWrite :: String -> ([String], [SeqTerm.R S] -> SeqTerm.M ()) -> IO ()
-- fpgaWrite name (portNames, mod) = writeFile (name ++ ".v") $ show $ v where
--   -- FIXME: see MyHDL.fpgaWrite
--   portNames' = map (\('_':nm) -> nm) portNames
--   portTypes = [SeqLib.bit | _ <- portNames]
--   v = Verilog.vModule name portNames' portTypes mod
  
