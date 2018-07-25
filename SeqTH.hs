-- Template Haskell rendering of a Seq program to remove
-- interpretative overhead.

-- Since Template Haskell is still a little unfamilar to me, I'm not
-- writing this as a tagless final interpreter, but as an explicit
-- compiler using SeqTerm

-- Interestingly, this doesn't need to be monadic.  The resulting
-- function can be constructed as pure  ((s,i)->(s,o))

{-# LANGUAGE TemplateHaskell #-}

module SeqTH(seqLam, seqLamTest) where
import SeqTerm
import Seq
import qualified SeqLib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List


seqLamTest = do
  closeMem [(SeqLib.bits 8)] $ \[rd] -> do
    en <- SeqTerm.input SeqLib.bit
    -- c <- SeqLib.counter (SInt (Just 4) 0)
    o <- SeqLib.integral en
    a <- SeqLib.counter $ SeqLib.bits 4
    b <- SeqLib.counter $ SeqLib.bits 5
    c <- SeqLib.counter $ SeqLib.bits 6
    
    return ([(en,a,b,c)], [o])

-- Abbrevs
type N = Op NodeNum
type T = Term N

data Part = D | I | MR | MW | E deriving Eq

-- Convert compiled Term to TH lambda expression
seqLam :: ([N], [(Int, T)]) -> Exp
seqLam  (outputs, bindings) = exp where

  -- Generate update function and initial values.
  exp = TupE [update, init]
  init = tupE' [memInit, stateInit]
  update =
    LamE [TupP [memIn, stateIn, inputs]] $
    LetE bindings' $
    TupE [memOut, stateOut, outputs']
  
  partition t = map snd $ filter ((t ==) . fst) tagged
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = I
  p (_, Delay _ _) = D
  p (_, MemRd _ _) = MR
  p (_, MemWr _)   = MW
  p _              = E
    
  bindings' =
    [ValD (nodeNumPat n) (NormalB (termExp e)) []
    | (n, e) <- partition E]
    
  inputs = tupP' $ map (nodeNumPat . fst) $ partition I

  outputs' = tupE' $ map nodeExp outputs

  ds = partition D
  stateInit = tupE' [int v | (_, (Delay (SInt _ v) _)) <- ds]
  stateIn   = tupP' $ map (nodeNumPat . fst) $ ds
  stateOut  = tupE' [nodeExp n | (_, (Delay _ n)) <- ds]

  mrs = partition MR
  mi _ = tupE' $ [int 0, var "InitMem"]
  mr (rd, MemRd _ (MemNode mem)) = tupP' [nodeNumPat rd, nodeNumPat mem]
  memInit  = tupE' $ map mi mrs
  memIn  = tupP' $ map mr mrs
  memOut =
    tupE' [AppE (var "UpdateMem") $
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
opVar opc = var $ show opc
var str = VarE $ mkName $ "seq" ++ str


termExp :: T -> Exp
termExp (Comb1 t opc a)     = app2 (opVar opc) (bits t) (nodeExp a)
termExp (Comb2 t opc a b)   = app3 (opVar opc) (bits t) (nodeExp a) (nodeExp b)
termExp (Comb3 t opc a b c) = app4 (opVar opc) (bits t) (nodeExp a) (nodeExp b) (nodeExp c)
--termExp (Slice _ a b c)     = app5 (opVar "SLICE") (nodeExp a) (nodeExp b) (nodeExp c)
termExp e = error $ show e

app1 = AppE
app2 a b c     = app1 (app1 a b) c
app3 a b c d   = app1 (app2 a b c) d
app4 a b c d e = app1 (app3 a b c d) e

bits (SInt (Just n) _) = int n


nodeExp :: N -> Exp          
nodeExp (Node _ n) = nodeNumExp n
nodeExp (Const (SInt _ v)) = int v

-- Fixme: should be Int
int i = AppE (var "Int") (LitE $ IntegerL $ fromIntegral i)
                 
nodeNumExp :: Int -> Exp          
nodeNumExp n = VarE $ nodeNumName n

nodeNumPat :: Int -> Pat          
nodeNumPat n = VarP $ nodeNumName n

nodeNumName :: Int -> Name
nodeNumName = mkName . nodeNumStr
nodeNumStr n = "r" ++ show n

