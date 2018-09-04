{-# LANGUAGE Rank2Types #-}

-- FIXME: Can't compile f_soc.hs yet, but f_blink.hs works.  Can't
-- find any more obvious errors.  Where to start debugging?  It's
-- likely the memory.

-- TODO:

-- . Create two generators: one for CLK,RST and one for just CLK.
--   This allows the iCE40 reset generator to be generated as well.

-- . Verilator has a C api.

-- The 'error' cases are projection errors.  FIXME: is there a simple
-- way to avoid these?  E.g. project partitions down to simpler data
-- types.


-- Since we're just generating RTL, there is a fairly direct mapping
-- from SeqTerm to Verilog.
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.Seq.Verilog where

import Language.Seq(S,SType(..),Op1(..),Op2(..),Op3(..),Seq(..))
import Language.Seq.NetList
import qualified Language.Seq.NetList as SeqNetList
import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Lib as SeqLib
import Data.AsmTools.PCF
import qualified Language.Seq.Test.Tools as TestTools

import Data.List hiding (partition)
import Numeric (showHex, showIntAtBase)
import Data.Char

import qualified Data.Map.Lazy as Map
import Data.Bits
import Control.Monad.Free
import Data.Graph
import Data.Tuple
import Data.List

-- Some simplifications:
-- . All signals are vectors.
-- . One block for registers, CLK, RST


data Verilog = Verilog String
instance Show Verilog where
  show (Verilog code) = code

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

data Variant =
  Module |   -- Ordinary Verilog module
  Cosim      -- Test bench bound to cosim.vpi

vModule = vModule' Module
  
vModule' :: Variant -> String -> [String] -> [SType] -> ([SeqTerm.R S] -> SeqTerm.M ()) -> Verilog
vModule' variant mod_name portNames portTypes mod = Verilog vCode where
  
  -- See SeqTerm for some post processing steps that are shared
  -- between HDLs.

  mod' = do
    -- Ports default as input.
    -- When assigned through 'update', type changes from in->out
    io <- SeqTerm.inputs $ portTypes
    mod io ; return io

  -- Graph algos only operate on Vertex
  netlist@(NetList ports formMap probes) = SeqNetList.compileTerm mod'
  dg = toDG formMap

  exprList = map unpack $ inlined dg where unpack (n, TypedExpr te) = (n, te)
  exprMap = Map.fromList exprList
  
  -- look up form and expression based on vertex
  refForm :: Vertex -> TypedForm Vertex
  refExpr :: Vertex -> TypedExpr' Vertex
  refForm = (formMap Map.!)
  refExpr = (exprMap Map.!)

  -- Keep port names.
  -- Annotate debug names with unique index.
  nameProbe (n, nm) = (n, nm ++ "_s" ++ show n)
  vertexNames = Map.fromList $ map swap $ (map nameProbe probes) ++ zip ports portNames
  vertexNames' = Map.fromList $ map swap $ Map.toList vertexNames
  vertexName n = Map.findWithDefault ("s" ++ show n) n vertexNames'
  
  part = partition' exprList

  sigDecl kind (Just sz) n =
    kind ++ " [" ++ show (sz-1) ++ ":0] " ++ vertexName n ++ ";"
  sigDecl _ _ n = error $ "sigDecl needs fixed bit size: " ++ vertexName n

  arrDecl (Just d_sz) n a_sz =
    "reg [" ++ show (d_sz-1) ++ ":0] " ++ vertexName n ++ "[0:" ++ show (a_sz-1) ++ "];"
  arrDecl _ n _ = error $ "arrDecl needs fixed bit size: " ++ vertexName n
  
  decl typ b@(n, (Free (TypedForm bits _))) =
    (sigDecl typ) bits n ++ debug b
  decl typ b = error $ "decl: " ++ show (typ,b)

  assign b@(n, term) =
    "assign " ++ vertexName n ++ " = " ++ op term ++ ";" ++
    debug b

  reset b@(n, Free (TypedForm t (Delay _ i))) =
    tab ++ tab ++ vertexName n ++ " <= " ++ literal (t,i) ++ ";" ++
    debug b
  reset b = error $ "reset: " ++ show b

  update b@(n, Free (TypedForm _ (Delay n' _))) =
    tab ++ tab ++ vertexName n ++ " <= " ++ (op n') ++ ";" ++
    debug b
  update b = error $ "update: " ++ show b
  
  literal (Nothing, v) = show v
  literal ((Just sz), v) = show sz ++ "'b" ++ bits where
    v' = v .&. ((1 `shiftL` sz)-1)
    bits' = showIntAtBase 2 intToDigit v' "" 
    bits  = replicate (sz - length bits') '0' ++ bits'

  op :: Free SeqNetList.TypedForm Vertex -> String
  op (Pure n) = vertexName n
  op (Free (TypedForm bits (Const v))) = literal (bits, v)
  op (Free (TypedForm bits f)) = expr f

  parens str = "(" ++ str ++ ")"

  expr (Comb1 INV o) = parens $ "~" ++ op o
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

  decls typ p = concat $ map (decl typ) $ p

  -- Associate Memory nodes with the corresponding Delay node.
  -- FIXME: Remove the delay nodes from register.
  mem_nodes :: [(Vertex, Vertex)]
  mem_nodes = map (memnode . fst) $ part Memories where
    memnode memn = (memn, deln) where
      deln = case fanout dg memn of
        [n] -> n
        [] -> error $ "memnode: no reads from data register. FIXME"
        fo -> error $ "memnode: fanout: " ++ show fo 

  noMemDelays = filter (\(n, _) -> notElem n $ map snd mem_nodes)

  
  mem_decls = concat $ map mem_decl mem_nodes
  mem_decl :: (Vertex, Vertex) -> String
  mem_decl (memn, deln) = str_del ++ str_arr where
    TypedForm tr@(Just data_bits) (Memory _ wa _ _) = refForm memn
    TypedForm (Just addr_bits) _ = refForm wa
    arr_size :: Int
    arr_size = 2 ^ addr_bits
    str_del = sigDecl "reg" tr deln    ++ (debug $ refForm deln) -- Read Data register
    str_arr = arrDecl tr memn arr_size ++ (debug $ refForm memn) -- Verilog Array

  mem_updates = concat $ map mem_update mem_nodes
  mem_update :: (Vertex, Vertex) -> String
  mem_update (memn, deln) = str_wr ++ str_rd where
    (Free (TypedForm _ (Memory we wa wd ra))) = refExpr memn
    str_rd = "always @(posedge CLK) begin\n" ++
             tab ++ vertexName deln ++ " <= " ++
             vertexName memn ++ "[" ++ op ra ++ "];" ++ (debug $ refForm deln) ++
             "end\n"
    str_wr = "always @(posedge CLK) begin\n" ++
             tab ++ "if (" ++ op we ++ ") begin\n" ++
             tab ++ tab ++ vertexName memn ++ "[" ++ op wa ++ "] <= " ++
             op wd ++ ";" ++ (debug $ refForm memn) ++
             tab ++ "end\n" ++
             "end\n"

  assigns = concat $ map assign $ (part Exprs ++ part Connects)

  delays = noMemDelays $ part Delays

  updates = concat $ map update delays
  resets  = concat $ map reset  delays

  -- block tab0 header lines =
  --   tab0 ++ header ++ " begin\n" ++
  --   concat $ [tab0 ++ tab ++ ++ line ++ "\n" | line <- lines] ++
  --   tab0 ++ "end\n"

  ios = map (vertexName . fst)
  inputs  = ios $ part Inputs
  outputs = ios $ part Connects

  vCode =
    (case variant of
      Module ->
        -- "`timescale 1ns/10ps\n" ++  -- from MyHDL output
        "module " ++ mod_name ++ "(" ++ commas (["CLK","RST"] ++ portNames) ++ ");\n" ++
        "input CLK;\n" ++
        "input RST;\n" ++
        decls "input"  (part Inputs) ++
        decls "output" (part Connects)
      Cosim ->
        "module " ++ mod_name ++ ";\n" ++
        "reg CLK;\n" ++
        "reg RST;\n" ++
        decls "reg"    (part Inputs) ++
        decls "wire"   (part Connects)
    ) ++
    decls "wire"   (part Exprs) ++
    decls "reg"    delays ++
    mem_decls ++
    assigns ++
    mem_updates ++
    "always @(posedge CLK, negedge RST) begin: SEQ\n" ++
    tab ++ "if (RST==0) begin\n" ++
    resets ++
    tab ++ "end\n" ++
    tab ++ "else begin\n" ++
    updates ++
    tab ++ "end\n" ++
    "end\n" ++

    (case variant of
       Module ->
         -- FIXME: reset generator should go somewhere else.
         "reg [7:0] reset_count;\n" ++
         "assign RST = (reset_count == 255);\n" ++
         "always @(posedge CLK) begin: RESET_GEN\n" ++
         tab ++ "if (!RST) begin\n" ++
         tab ++ tab ++ "reset_count <= (reset_count + 1);\n" ++
         tab ++ "end\n" ++
         "end\n"
       Cosim ->
         "initial begin\n" ++
         tab ++ "$to_seq(" ++ commas outputs ++ ");\n" ++
         tab ++ "$from_seq(" ++ commas inputs ++ ");\n" ++
         tab ++ "RST <= 0;\n" ++
         tab ++ "CLK <= 0;\n" ++
         tab ++ "#1 RST <= 1;\n" ++
         "end\n" ++
         "always @(posedge CLK) begin\n" ++
         tab ++ "$tick_seq;\n" ++
         "end\n" ++
         "always\n" ++
         tab ++ "#5 CLK = ~CLK;\n"
    ) ++
    "endmodule\n"
    

tab = "    "
-- debug t = " // " ++ show t ++ "\n"
debug _ = "\n"


commas = intercalate ", "





fpgaGen name (names, fun) pins = (v, pcf') where 
  names' = map (\('_':nm) -> nm) names
  v = vModule name names' types fun
  types = [SInt (Just 1) 0 | _ <- names]
  pcf' = PCF ("CLK":"RST":names') pins

fpgaWrite name mod pins = do
  let (v,pcf) = fpgaGen name mod pins
  writeFile (name ++ ".v")   $ show v
  writeFile (name ++ ".pcf") $ show pcf

testbench ::
  String
  -> [Int]
  -- Rank 2, because we instantiate it twice.
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]]
  -> Verilog


testbench mod_name inSizes mod inputs = hdl where
  -- tb = TestBench hdl input $ Just $ output
  (portNames, portTypes, mod', outputs) =
    TestTools.testbench mod_name inSizes mod inputs
  hdl = vModule mod_name portNames portTypes mod'


testbench' ::
  String
  -> [Int]
  -- Rank 2, because we instantiate it twice.
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]]
  -> Verilog

testbench' mod_name inSizes mod inputs = hdl where
  -- tb = TestBench hdl input $ Just $ output
  (portNames, portTypes, mod', outputs) =
    TestTools.testbench mod_name inSizes mod inputs
  hdl = vModule' Cosim mod_name portNames portTypes mod'

