-- Template Haskell rendering of a Seq program to remove
-- interpretative overhead.

-- Since Template Haskell is untyped, it's OK to just use SeqTerm
-- instead of a tagless final compiler.

-- Memories are best implemented imperatively, in the ST monad.

-- Note that without memories, language can be compiled to a pure
-- state update function ((s,i)->(s,o))


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

data Part = Delays | Inputs | MemRds | MemWrs | Exprs deriving Eq

-- Convert compiled Term to TH expression
toExp :: ([O], [(Int, Term O)]) -> Exp
toExp  (outputs, bindings) = exp where

  -- Generate update function and initial values.

  -- The rank-2 types impose some constraints.  I made it work by 1)
  -- generating a closed expression to avoid "impredicative
  -- polymorphism" errors, and 2) placing the arrays in a list and
  -- giving them all the same type, instead of a tuple.
  
  exp = app3
    (seqVar "Run")
    update
    memSpec $
    TupE [memRdInit, stateInit]
  
  update =
    LamE [TupP [memRef, memRdIn, stateIn, inputs]] $
    DoE $
    bindings' ++
    memUpdate ++ 
    [NoBindS $ return' $ TupE [memRdOut, stateOut, outputs']]

  partition t = map snd $ filter ((t ==) . fst) tagged
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = Inputs
  p (_, Delay _ _) = Delays
  p (_, MemRd _ _) = MemRds
  p (_, MemWr _)   = MemWrs
  p _              = Exprs
    
  bindings' =
    [BindS (regP n) (termExp e)
    | (n, e) <- partition Exprs]

  -- I/O is more conveniently exposed as lists, which would be the
  -- same interface as the source code.  State can use tuples: it will
  -- be treated as opaque.

  inputs   = ListP $ map (regP . fst)  $ partition Inputs
  outputs' = ListE $ map opE outputs

  delays = partition Delays
  stateInit = tupE' [int v | (_, (Delay (SInt _ v) _)) <- delays]
  stateIn   = tupP' $ map (regP . fst) $ delays
  stateOut  = tupE' [opE n | (_, (Delay _ n)) <- delays]


  -- Keep track of arr to rData mapping.
  ards = [(op2reg op, n) | (n, (MemRd _ op)) <- partition MemRds]
  arrays  = map fst ards
  rDatas  = map snd ards
  arr2rData = Map.fromList ards

  memSpec   = ListE [ int 123 | _ <- rDatas ]  -- FIXME: sizes!
  memRef    = ListP $ map regP arrays
  memRdInit = tupE' [ int 0 | _ <- rDatas ]
  memRdIn   = tupP' $ map regP  rDatas
  memRdOut  = tupE' $ map regE' rDatas
  memUpdate =
    [BindS
      (regP' $ arr2rData Map.! arr)
      (app2
       (seqVar "MemUpdate")
       (regE arr)
       (tupE' $ map opE [rEn,wAddr,wData,rAddr]))
    | (arr, (MemWr (rEn,wAddr,wData,rAddr))) <- partition $ MemWrs]



-- FIXME: Use nested tuples for large state.  Maybe it is even OK to
-- put this in a list.  Check assembly output.

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
  exp = app4 (opVar CONC) (bits t) (bits' b) (opE a) (opE b)
termExp (Slice t a _ r) =
  app3 (seqVar "SLICE") (bits t) (opE a) (int r)
-- Generic 1,2,3 op
termExp (Comb1 t opc a)     = app2 (opVar opc) (bits t) (opE a)
termExp (Comb2 t opc a b)   = app3 (opVar opc) (bits t) (opE a) (opE b)
termExp (Comb3 t opc a b c) = app4 (opVar opc) (bits t) (opE a) (opE b) (opE c)

termExp e = error $ show e

app1 = AppE
app2 a b c     = app1 (app1 a b) c
app3 a b c d   = app1 (app2 a b c) d
app4 a b c d e = app1 (app3 a b c d) e

bits (SInt (Just n) _) = int n
bits _ = int 64 -- FIXME

-- Node (register) numbers appear in two contexts: Op and plain.
-- Instead of specializing functions to both, we take NodeNum as the canonical representation.
op2reg :: Op NodeNum -> NodeNum
op2reg (Node _ n) = n
op2reg (MemNode n) = n
op2reg c@(Const _) = error $ "op2reg: expecting Node reference: " ++ show c

opE :: O -> Exp          
opE (Node _ n) = regE n
opE (Const (SInt _ v)) = int v


int i = AppE (seqVar "Int") (LitE $ IntegerL $ fromIntegral i)
                 
regE  = VarE . regN
regE' = VarE . regN'

regP  = VarP . regN
regP' = VarP . regN'

regN  = mkName . regStr
regN' = mkName . regStr'

regStr  n = "r" ++ show n
regStr' n = "r" ++ show n ++ "'"

    
return' = AppE (VarE $ mkName "return")

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
