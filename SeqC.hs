-- FIXME: Proof-of-concept stub
module SeqC where

import Seq
import SeqTerm
import Data.List

newtype C = C (String,([Op NodeNum], [(NodeNum, Term (Op NodeNum))]))

showL :: Show a => [a] -> String
showL = concat . (map ((++ "\n") . show))

compile funName (outputs, bindings) = code where
  part = SeqTerm.partition bindings
  inputs = part Inputs
  -- FIXME: Only combinatorial for now
  -- delays = part Delays
  -- memrds = part MemRds
  -- memwrs = part MemWrs
  exprs  = part Exprs

  reg  n = "r"  ++ show n
  oreg i = "o[" ++ show i ++ "]"

  cInputDecl (n, Input (SInt _ _)) = intType ++ " " ++ reg n
  cBinding (n, e) = "\t" ++ intType ++ " " ++ reg n ++ " = " ++ cExpr e ++ ";\n"
  cExpr (Comb2 _ op a b) = cPrim (show op) [a,b]
  cOperand (Const (SInt _ v)) = show v
  cOperand (Node _ n) = reg n
  cOperands os = argList $ map cOperand os
  cPrim opcode operands = "seq" ++ opcode ++ cOperands operands
    
  cOutputAssign i opr ="\t" ++ oreg i ++ " = " ++ cOperand opr ++ ";\n"

  argList as = "(" ++ (concat $ intersperse ", " as) ++ ")"

  intType = "seq_t"
  
  code =
    "static inline void " ++ funName ++
    (argList  $ [intType ++ " " ++ oreg (length outputs)] ++ map cInputDecl inputs) ++
    " {\n" ++
    (concat $ map cBinding exprs) ++
    (concat $ zipWith cOutputAssign [0..] outputs) ++
    "}\n"
  

instance Show C where
  -- show (C (outputs, bindings)) = (showL outputs) ++ (showL bindings)
  show (C (funName, ct)) = compile funName ct

