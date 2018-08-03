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
import qualified Data.Map.Strict as Map

test [en] = do
  closeMem [(SeqLib.bits 8)] $ \[rd] -> do
    o <- SeqLib.integral en
    a <- SeqLib.counter $ SeqLib.bits 4
    b <- SeqLib.counter $ SeqLib.bits 5
    c <- SeqLib.counter $ SeqLib.bits 6
    return ([(en,a,b,c)], [o])

-- Abbrevs
type O = Op NodeNum

data Part = D | I | MR | MW | E deriving Eq

-- Convert compiled Term to TH expression
toExp :: ([O], [(Int, Term O)]) -> Exp
toExp  (outputs, bindings) = exp where

  -- Generate update function and initial values.

  -- Note: I've been running into "impredicative polymorphism" errors
  -- trying to make the loop function and initial state explicit,
  -- which I don't want to understand yet.  It seems best to just
  -- generate a closed expression.
  
  
  exp = app3 (seqVar "Run") update memInit $ TupE [memRdInit, stateInit]
  update =
    LamE [TupP [arrP, memRdIn, stateIn, inputs]] $
    DoE $
    bindings' ++
    memUpdate ++ 
    [NoBindS $ return' $ TupE [memRdOut, stateOut, outputs']]

  
  partition t = map snd $ filter ((t ==) . fst) tagged
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = I
  p (_, Delay _ _) = D
  p (_, MemRd _ _) = MR
  p (_, MemWr _)   = MW
  p _              = E
    
  bindings' =
    [BindS (rP n) (termExp e)
    | (n, e) <- partition E]

  -- I/O is more conveniently exposed as lists, which would be the
  -- same interface as the source code.  State can use tuples: it will
  -- be treated as opaque.

  inputs   = ListP $ map (rP . fst)  $ partition I
  outputs' = ListE $ map oE outputs

  ds = partition D
  stateInit = tupE' [int v | (_, (Delay (SInt _ v) _)) <- ds]
  stateIn   = tupP' $ map (rP . fst) $ ds
  stateOut  = tupE' [oE n | (_, (Delay _ n)) <- ds]


  -- TODO:
  -- . initializers
  -- . seqUpdateMem as monadic operation
  -- . rData feedback (but not arr)

  -- Variable declarations (P) and references (E)

  -- We'll need to create another node for the intermediate result of
  -- MemWr, so maintain an assoc list.
  ards = [(o2n on, n) | (n, (MemRd _ on)) <- partition MR]
  arr2rData = Map.fromList ards
  
  as = map fst ards
  rds = map snd ards

  -- Reuse it for the register tuples
  arrP    = ListP $ map rP  as


  memUpdate =
    [BindS
      (rP' $ arr2rData Map.! arr)
      (app2
       (seqVar "MemUpdate")
       (rE arr)
       (tupE' $ map oE [rEn,wAddr,wData,rAddr]))
    | (arr, (MemWr (rEn,wAddr,wData,rAddr))) <- partition $ MW]

  -- It's simplest to just feed back the read data register.
  memRdInit = tupE' [ int 0            | _ <- rds ]
  memRdIn   = tupP' $ map rP rds
  memRdOut  = tupE' $ map rE' rds

  memInit   = ListE [ int 0 | _ <- rds ]


  -- memOut = tupE' $ 
  --   tupE' [AppE (seqVar "UpdateMem") $
  --           TupE [tupE' $ map oE [a,b,c,d],
  --                 rE n]
  --         | (n, (MemWr (a,b,c,d))) <- partition MW]

  -- memOut =
  --   tupE' [AppE (seqVar "UpdateMem") $
  --           TupE [tupE' $ map oE [a,b,c,d],
  --                 rE n]
  --         | (n, (MemWr (a,b,c,d))) <- partition MW]
    

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


termExp :: Term O -> Exp

-- Special cases
termExp (Comb2 t CONC a b) = exp where
  bits' n = int b where (SInt (Just b) _) = opType n
  exp = app4 (opVar CONC) (bits t) (bits' b) (oE a) (oE b)
termExp (Slice t a _ r) =
  app3 (seqVar "SLICE") (bits t) (oE a) (int r)
-- Generic 1,2,3 op
termExp (Comb1 t opc a)     = app2 (opVar opc) (bits t) (oE a)
termExp (Comb2 t opc a b)   = app3 (opVar opc) (bits t) (oE a) (oE b)
termExp (Comb3 t opc a b c) = app4 (opVar opc) (bits t) (oE a) (oE b) (oE c)

termExp e = error $ show e

app1 = AppE
app2 a b c     = app1 (app1 a b) c
app3 a b c d   = app1 (app2 a b c) d
app4 a b c d e = app1 (app3 a b c d) e

bits (SInt (Just n) _) = int n
bits _ = int 64 -- FIXME

-- Node numbers appear in two contexts: Op and plain.
-- Instead of specializing functions to both, we take NodeNum as the canonical representation.
o2n :: Op NodeNum -> NodeNum
o2n (Node _ n) = n
o2n (MemNode n) = n
o2n c@(Const _) = error $ "o2n: expecting Node reference: " ++ show c

oE :: O -> Exp          
oE (Node _ n) = rE n
oE (Const (SInt _ v)) = int v


int i = AppE (seqVar "Int") (LitE $ IntegerL $ fromIntegral i)
                 
rE  = VarE . rN
rE' = VarE . rN'

rP  = VarP . rN
rP' = VarP . rN'

rN  = mkName . rStr
rN' = mkName . rStr'

rStr  n = "r" ++ show n
rStr' n = "r" ++ show n ++ "'"

    
return' = AppE (VarE $ mkName "return")

-- sequenceTupE :: String -> [Exp] -> Exp
-- sequenceTupE prefix ms = e where
--   v n = mkName $ prefix ++ show n
--   e = DoE $
--       [BindS (VarP $ v n) m | (n,m) <- zip [0..] ms] ++
--       [NoBindS $ return' $ tupE' [VarE $ v n | (n,m) <- zip [0..] ms]]

compile' :: [Int] -> ([R S] -> M [R S]) -> Exp
compile' sizes mf = exp where
  -- SeqPrim expects internal values to be truncated.
  -- Perform a slice operation to assure this.
  truncIns = sequence $ map truncIn sizes
  truncIn sz = do
    i <- input $ SInt (Just sz) 0
    slice i (Just sz) 0
  exp = toExp $ SeqTerm.compileTerm $ truncIns >>= mf

compile sizes mf = return $ compile' sizes mf



-- second stage
run = SeqPrim.seqRun


-- Change it to run in ST, returning the value of the memories as well.
