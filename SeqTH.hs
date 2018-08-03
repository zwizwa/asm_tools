-- Template Haskell rendering of a Seq program to remove
-- interpretative overhead.

-- Since Template Haskell is still a little unfamiliar to me, I'm not
-- writing this as a tagless final interpreter, but as an explicit
-- compiler using SeqTerm

-- Interestingly, this doesn't need to be monadic.  The resulting
-- function can be constructed as pure  ((s,i)->(s,o))

{-# LANGUAGE TemplateHaskell #-}

module SeqTH(toExp, compile', compile, run, test) where

import Seq
import SeqTerm
import SeqPrim
import qualified SeqTerm
import qualified SeqLib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List


test [en] = do
  closeMem [(SeqLib.bits 8)] $ \[rd] -> do
    o <- SeqLib.integral en
    a <- SeqLib.counter $ SeqLib.bits 4
    b <- SeqLib.counter $ SeqLib.bits 5
    c <- SeqLib.counter $ SeqLib.bits 6
    return ([(en,a,b,c)], [o])

-- Abbrevs
type N = Op NodeNum
type T = Term N

data Part = D | I | MR | MW | E deriving Eq

-- Convert compiled Term to TH expression
toExp :: ([N], [(Int, T)]) -> Exp
toExp  (outputs, bindings) = exp where

  -- Generate update function and initial values.

  -- Note: I've been running into "impredicative polymorphism" errors
  -- trying to make the loop function and initial state explicit,
  -- which I don't want to understand yet.  It seems best to just
  -- generate a closed expression.
  
  
  exp = app2 (seqVar "Run") update init
  
  init = TupE [memInit, stateInit]
  update =
    LamE [TupP [memIn, stateIn, inputs]] $
    DoE $
    bindings' ++
    [NoBindS $ AppE
     (VarE $ mkName "return")
     (TupE [stateOut, outputs'])]
  
  partition t = map snd $ filter ((t ==) . fst) tagged
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = I
  p (_, Delay _ _) = D
  p (_, MemRd _ _) = MR
  p (_, MemWr _)   = MW
  p _              = E
    
  bindings' =
    [BindS (nodeNumPat n) (termExp e)
    | (n, e) <- partition E]

  -- I/O is more conveniently exposed as lists, which would be the
  -- same interface as the source code.  State can use tuples: it will
  -- be treated as opaque.

  inputs   = ListP $ map (nodeNumPat . fst)  $ partition I
  outputs' = ListE $ map nodeExp outputs

  ds = partition D
  stateInit = tupE' [int v | (_, (Delay (SInt _ v) _)) <- ds]
  stateIn   = tupP' $ map (nodeNumPat . fst) $ ds
  stateOut  = tupE' [nodeExp n | (_, (Delay _ n)) <- ds]

  mrs = partition MR
  mi _ = tupE' $ [int 0, seqVar "InitMem"]
  mr (rd, MemRd _ (MemNode mem)) =
    tupP' [nodeNumPat rd, nodeNumPat mem]
  memInit  = tupE' $ map mi mrs
  memIn  = tupP' $ map mr mrs
  memOut =
    tupE' [AppE (seqVar "UpdateMem") $
            TupE [tupE' $ map nodeExp [a,b,c,d],
                  nodeNumExp n]
          | (n, (MemWr (a,b,c,d))) <- partition MW]


-- FIXME: Use nested tuples for the state, memory collections.

tupE' :: [Exp] -> Exp
tupE' [a] = a
tupE' as = TupE as

tupP' :: [Pat] -> Pat
tupP' [a] = a
tupP' as = TupP as

-- Primitive operation names
opVar :: Show t => t -> Exp
opVar opc = seqVar $ show opc
seqVar str = VarE $ mkName $ "seq" ++ str


termExp :: T -> Exp

-- Special cases
termExp (Comb2 t CONC a b) = exp where
  bits' n = int b where (SInt (Just b) _) = opType n
  exp = app4 (opVar CONC) (bits t) (bits' b) (nodeExp a) (nodeExp b)
termExp (Slice t a _ r) =
  app3 (seqVar "SLICE") (bits t) (nodeExp a) (int r)
-- Generic 1,2,3 op
termExp (Comb1 t opc a)     = app2 (opVar opc) (bits t) (nodeExp a)
termExp (Comb2 t opc a b)   = app3 (opVar opc) (bits t) (nodeExp a) (nodeExp b)
termExp (Comb3 t opc a b c) = app4 (opVar opc) (bits t) (nodeExp a) (nodeExp b) (nodeExp c)

termExp e = error $ show e

app1 = AppE
app2 a b c     = app1 (app1 a b) c
app3 a b c d   = app1 (app2 a b c) d
app4 a b c d e = app1 (app3 a b c d) e

bits (SInt (Just n) _) = int n
bits _ = int 64 -- FIXME


nodeExp :: N -> Exp          
nodeExp (Node _ n) = nodeNumExp n
nodeExp (Const (SInt _ v)) = int v

int i = AppE (seqVar "Int") (LitE $ IntegerL $ fromIntegral i)
                 
nodeNumExp :: Int -> Exp          
nodeNumExp n = VarE $ nodeNumName n

nodeNumPat :: Int -> Pat          
nodeNumPat n = VarP $ nodeNumName n

nodeNumName :: Int -> Name
nodeNumName = mkName . nodeNumStr
nodeNumStr n = "r" ++ show n

compile' :: [Int] -> ([R S] -> M [R S]) -> Exp
compile' sizes mf = exp where
  -- SeqPrim expects internal values to be truncated.
  -- Perform a slice operation to assure this.
  truncIns = sequence $ map truncIn sizes
  truncIn sz = do
    i <- input $ SInt (Just sz) 0
    slice i (Just sz) 0
  exp = toExp $ SeqTerm.compileTerm $ truncIns >>= mf

compile sizes mf = return $
  compile' sizes mf



-- second stage
run = SeqPrim.seqRun





-- Change it to run in ST, returning the value of the memories as well.
