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
import qualified Data.Map.Lazy as Map
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
  p' x@(n, (Free (TypedForm _ form))) = (p form, x)
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
  dg = toDG bindmap

  bindings = map unpack $ inlined dg where
    unpack (n, TypedExpr te) = (n, te)
  
  
  -- Convert node rep to String
  -- bindings = map nameBinding $ inlined dag where
  --   -- FIXME: use probe names
  --   nameBinding (n, e) = (nameNode n, fmap nameNode e)

  nameNode = ("s" ++) . show
  
  part = partition' bindings

  portSpecs = undefined

  
  
  sigDecl kind (Just sz) n =
    kind ++ " [" ++ show (sz-1) ++ ":0] " ++ nameNode n ++ ";"
  sigDecl _ _ n = error $ "sigDecl needs fixed bit size: " ++ nameNode n
  
  decl typ b@(n, (Free (TypedForm bits _))) =
    (sigDecl typ) bits n ++ debug b
  -- decl typ b = error $ "decl: " ++ show (typ,b)


  assign b@(n, term) =
    "assign " ++ nameNode n ++ " = " ++ op term ++ ";" ++
    debug b

  reset b@(n, Free (TypedForm t (Delay _ i))) =
    tab ++ tab ++ nameNode n ++ " <= " ++ literal (t,i) ++ ";" ++
    debug b
  reset b = proj_err b

  update b@(n, Free (TypedForm _ (Delay n' _))) =
    tab ++ tab ++ nameNode n ++ " <= " ++ (op n') ++ ";" ++
    debug b
  update b = proj_err b
  
  literal (Nothing, v) = show v
  literal ((Just sz), v) = show sz ++ "'b" ++ bits where
    v' = v .&. ((1 `shiftL` sz)-1)
    bits' = showIntAtBase 2 intToDigit v' "" 
    bits  = replicate (sz - length bits') '0' ++ bits'

  op :: Free SeqNetList.TypedForm Vertex -> String
  op (Pure n) = nameNode n
  op (Free (TypedForm bits (Const v))) = literal (bits, v)
  op (Free (TypedForm bits f)) = expr f

  parens str = "(" ++ str ++ ")"

  expr (Comb1 INV o) = parens $ "!" ++ op o
  expr (Comb2 ADD a b) = op2 "+" a b
  expr (Comb2 SUB a b) = op2 "-" a b
  expr (Comb2 MUL a b) = op2 "*" a b
  expr (Comb2 AND a b) = op2 "&" a b
  expr (Comb2 OR  a b) = op2 "|" a b
  expr (Comb2 XOR a b) = op2 "^" a b
  expr (Comb2 SLL a b) = op2 "<<" a b
  expr (Comb2 SLR a b) = op2 ">>" a b
  expr (Comb2 CONC a b) = "{" ++ op a ++ ", " ++ op b ++ "}"
  expr (Comb2 EQU a b) = op2 "==" a b
  expr (Comb3 IF a b c) = op a ++ " ? " ++ op b ++ " : " ++ op c
  expr (Connect o) = op o
  expr (Slice o (Just u) l) = op o ++ "[" ++ show (u-1) ++ ":" ++ show l ++ "]"
  expr e = "... /* " ++ show e ++ " */ "
  
  op2 opc a b = parens (op a ++ " " ++ opc ++ " " ++ op b)





  -- For Verilog, but likely necessary for other HDLs.  In the Form
  -- language, memories are represented as a read data register (Delay)
  -- and a memory lookup combinatorial network (Memory).  Those need to
  -- be identified such that the memory storage can be declared properly.



  
  --(portSpecs, (ports, bindings')) =
  --  SeqTerm.hdl_compile' portNames portTypes mod


  -- Convert ports' bindings' to Verilog syntax

  decls typ p = concat $ map (decl typ) $ part p

  
  -- Bundle Delay and Memory nodes.  These are not inlined, so use the
  -- original form, a little easier.
  memnodes :: [((Vertex, TypedForm Vertex),
                (Vertex, TypedForm Vertex))]
  memnodes = take 0 $ map memnode $ part Memories where
    memnode (n, e) = ((n, bindmap Map.! n), (n', e')) where
      n' = case fanout dg n of
        [n'] -> n'
        -- Shows up as [] sometimes. bug?
        fo -> error $ "memnode: fanout: " ++ show fo 
      e' = bindmap Map.! n'
  
  mem_decls = concat $ map memory_decl memnodes
  memory_decl :: ((Vertex, TypedForm Vertex), (Vertex, TypedForm Vertex)) -> String
  memory_decl args@((memn, (TypedForm _ (Memory we wa wd ra))), (deln, (TypedForm _ (Delay _ _)))) =
    "// " ++ show args ++ "\n"
    -- FIXME
    -- let arrSize = 2 ^ n
    --     (SInt (Just n) _) = opType wa
    -- in
    --   -- Note: the _ra signal is derived from the memory name.
    --   sigDecl "reg" t name ++ debug b
    --   arrDecl (opType wd) mem_name arrSize ++
    --   debug b ++
    --   sigDecl "wire" (opType ra) (mem_name ++ "_ra") ++
    --   debug b


  assigns = concat $ map assign $ (part Exprs ++ part Connects)

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
    mem_decls ++
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
    
  -- vCode =
  --   memwr_assigns ++
  --   memrd_updates ++
  --   memwr_updates ++


tab = "    "
debug t = " // " ++ show t ++ "\n"


-- debug _ = "//\n"

-- arrDecl (SInt (Just n) _) name sz =
--   "reg [" ++ show (n-1) ++ ":0] " ++ name ++ "[0:" ++ show (sz-1) ++ "];"
-- arrDecl _ name _ = error $ "arrDecl needs fixed bit size: " ++ name



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


commas = intercalate ", "

