-- FIXME: Proof-of-concept stub
module SeqC where

import Seq
import SeqTerm
import Data.List

newtype C = C ([Op NodeNum], [(NodeNum, Term (Op NodeNum))])

showL :: Show a => [a] -> String
showL = concat . (map ((++ "\n") . show))

compile (outputs, bindings) = code where
  part = SeqTerm.partition bindings
  inputs = part Inputs
  -- FIXME: Only combinatorial for now
  -- delays = part Delays
  -- memrds = part MemRds
  -- memwrs = part MemWrs
  exprs  = part Exprs

  reg n = "r" ++ show n

  cInputDecl (n, Input (SInt _ _)) = intType ++ " " ++ reg n
  cBinding (n, e) = "\t" ++ reg n ++ " = " ++ cExpr e ++ ";\n"
  cExpr (Comb2 _ op a b) = cPrim (show op) [a,b]
  cOperand (Const (SInt _ v)) = show v
  cOperand (Node _ n) = reg n
  cOperands os = argList $ map cOperand os
  cPrim opcode operands = "seq" ++ opcode ++ cOperands operands
    
  cOutputAssign (Node _ n) = "\t*o_" ++ reg n ++ " = " ++ reg n ++ ";\n"
  cOutputDecl (Node _ n) = intType ++ " *o_" ++ reg n

  argList as = "(" ++ (concat $ intersperse ", " as) ++ ")"

  funName = "fun"
  intType = "T"
  
  code =
    "void " ++ funName ++
    (argList  $ (map cInputDecl inputs) ++ (map cOutputDecl outputs)) ++
    " {\n" ++
    (concat $ map cBinding exprs) ++
    (concat $ map cOutputAssign outputs) ++
    "}\n"
  

instance Show C where
  -- show (C (outputs, bindings)) = (showL outputs) ++ (showL bindings)
  show (C ct) = compile ct

