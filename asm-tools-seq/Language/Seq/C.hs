-- History:
--
-- . Original driver was the need for a simple C code generator to
--   generate some parameter conversions (2018/7?)
-- . Seq was extended to include MUL
-- . Later (2019/4) this code acts as a driver to add loops

{-# LANGUAGE TypeSynonymInstances #-}

module Language.Seq.C where

import Language.Seq
import Language.Seq.Term

import Data.List

newtype C = C (String, CompileResult NodeNum)

showL :: Show a => [a] -> String
showL = concat . (map ((++ "\n") . show))

-- Several targets need different partitioning, so copy paste and edit.
data Part = Delays | Inputs | MemRds | MemWrs | Exprs deriving Eq
partition' bindings t = map snd $ filter ((t ==) . fst) tagged where
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = Inputs
  p (_, Delay _ _) = Delays
  p (_, MemRd _ _) = MemRds
  p (_, MemWr _)   = MemWrs
  p _              = Exprs


-- compile :: (ArrayLoop n, Show n) => String -> CompileResult n -> String
compile :: Show n => String -> CompileResult n -> String
compile funName (outputs, bindings, _) = code where
  part = partition' bindings
  inputs = part Inputs
  -- FIXME: Only combinatorial for now
  -- delays = part Delays
  -- memrds = part MemRds
  -- memwrs = part MemWrs
  exprs  = part Exprs

  reg  n = "r"  ++ show n
  oreg i = "o[" ++ show i ++ "]"

  -- Recursive binding for loops over arrays is implemented abstractly
  -- using the ArrayLoop typeclass.  This way it can be embedded in
  -- node types.
  
  cBinding (n, e) = "\t" ++ intType ++ " " ++ reg n ++ " = " ++ cExpr e ++ ";\n"

  cInputDecl (n, Input (SInt _ _)) = intType ++ " " ++ reg n
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


-- FIXME: Loops are expressed as a recursive extension to the basic
-- flat Term structure.
--class ArrayLoop n
--instance ArrayLoop NodeNum
