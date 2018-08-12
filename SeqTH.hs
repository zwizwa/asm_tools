-- Template Haskell rendering of a Seq program to remove
-- interpretative overhead.

-- Since Template Haskell is untyped, it's OK to just use SeqTerm
-- instead of a tagless final compiler.

-- Memories are best implemented imperatively, in the ST monad.

-- Note that without memories, language can be compiled to a pure
-- state update function ((s,i)->(s,o))


{-# LANGUAGE TemplateHaskell #-}

module SeqTH(toExp, compile', compile, test, noProbe) where

import Seq
import SeqLib hiding (bits)
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


-- Convert compiled Term to TH expression
toExp :: ([Op NodeNum], [(Int, Term (Op NodeNum))], [(Op NodeNum, String)]) -> Exp
toExp  (outputs, bindings, probes) = exp where

  -- Generate update function and initial values.

  -- The rank-2 types impose some constraints.  I made it work by 1)
  -- generating a closed expression to avoid "impredicative
  -- polymorphism" errors, and 2) placing the arrays in a list and
  -- giving them all the same type, instead of a tuple.
  
  exp = app4
    (seqVar "Run")
    update
    memSpec 
    (TupE [memRdInit, stateInit])
    (ListE $ map (LitE . StringL . snd) probes)

  update =
    LamE [TupP [memRef, memRdIn, stateIn, inputs]] $
    DoE $
    bindings' ++
    memUpdate ++ 
    [NoBindS $ return' $ TupE [memRdOut, stateOut, outputs']]

  partition = SeqTerm.partition bindings
    
  bindings' =
    [BindS (regP n) (termExp e)
    | (n, e) <- partition Exprs]

  -- I/O is more conveniently exposed as lists, which would be the
  -- same interface as the source code.  Probe outputs are just
  -- appended.

  inputs   = ListP $ map (regP . fst)  $ partition Inputs
  outputs' = ListE $ (map opE outputs ++ map (opE . fst) probes)

  -- State can use tuples: it will be treated as opaque.
  
  delays = partition Delays
  stateInit = tupE' [int v | (_, (Delay (SInt _ v) _)) <- delays]
  stateIn   = tupP' $ map (regP . fst) $ delays
  stateOut  = tupE' [opE n | (_, (Delay _ n)) <- delays]

  -- Memories.

  memrds = partition MemRds
  memwrs = partition MemWrs

  arrays   = [op2reg op | (_, (MemRd _ op)) <- memrds]
  rDatas   = [n         | (n, (MemRd _ _ )) <- memrds]

  arr2rData = Map.fromList $ zip arrays rDatas
  arr2size  = Map.fromList $ [(arr, opBits wAddr)
                             | (arr, (MemWr (_,wAddr,_,_))) <- memwrs]

  memSpec   = ListE [ int $ arr2size Map.! a | a <- arrays ]
  memRef    = ListP $ map regP arrays
  memRdInit = tupE' [ int 0 | _ <- rDatas ]
  memRdIn   = tupP' $ map regP  rDatas
  memRdOut  = tupE' $ map regE' rDatas
  memUpdate =
    [BindS
      (regP' $ arr2rData Map.! arr)
      (app3
       (seqVar "MemUpdate")
       (regE arr)
       (int $ opBits wData)
       (tupE' $ map opE [rEn,wAddr,wData,rAddr]))
    | (arr, (MemWr (rEn,wAddr,wData,rAddr))) <- memwrs]



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


termExp :: Term (Op NodeNum) -> Exp

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

bits :: SType -> Exp
bits = int . tbits

tbits (SInt (Just n) _) = n
tbits (SInt Nothing _) = -1 -- See SeqPrim

opBits :: Op NodeNum -> Int
opBits (Node t _) = tbits t
opBits (Const t) = tbits t



-- Node (register) numbers appear in two contexts: Op and plain.
-- Instead of specializing functions to both, we take NodeNum as the canonical representation.
op2reg :: Op NodeNum -> NodeNum
op2reg (Node _ n) = n
op2reg (MemNode n) = n
op2reg c@(Const _) = error $ "op2reg: expecting Node reference: " ++ show c

opE :: Op NodeNum -> Exp          
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

compile' :: (String -> Bool) -> [Int] -> ([R S] -> M [R S]) -> Exp
compile' selectProbe inSizes mf = exp where
  -- SeqPrim expects internal values to be truncated.
  -- Perform a slice operation to assure this.
  mf' = truncIns >>= mf
  truncIns = sequence $ map truncIn inSizes
  truncIn sz = do
    i <- input $ SInt (Just sz) 0
    slice i (Just sz) 0

  (ports, bindings, probes) = SeqTerm.compileTerm' $ mf'
  probes' = filter (selectProbe . snd) probes
  exp = toExp (ports, bindings, probes')


compile :: (String -> Bool) -> [Int] -> ([R S] -> M [R S]) -> Q Exp
compile probes inSizes mf = return $ compile' probes inSizes mf

noProbe :: String -> Bool
noProbe = const False
