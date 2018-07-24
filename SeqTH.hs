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
    i <- SeqTerm.input SeqLib.bit
    -- c <- SeqLib.counter (SInt (Just 4) 0)
    o <- SeqLib.integral i
    return [o]

-- Abbrevs
type N = Op NodeNum
type T = Term N

data Part = D | I | MR | MW | E deriving Eq

-- Convert compiled Term to TH lambda expression
seqLam :: ([N], [(Int, T)]) -> Exp
seqLam  (outputs, bindings) = exp where
  exp = LamE [TupP [s, i]] $ LetE bs $ TupE [s',o]
  bs = [ValD (nodeNumPat n) (NormalB (termExp e)) [] | (n, e) <- partition E]
  o = tupE' $ map nodeExp outputs

  partition t = map snd $ filter ((t ==) . fst) tagged
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = I
  p (_, Delay _ _) = D
  p (_, MemRd _ _) = MR
  p (_, MemWr _)   = MW
  p _              = E
    
  ds = partition D
  s  = tupP' $ map (nodeNumPat . fst) $ ds
  s' = tupE' [nodeExp n | (_, (Delay _ n)) <- ds]

  i = tupP' $ map (nodeNumPat . fst) $ partition I

  -- mr =
  -- mw



tupE' :: [Exp] -> Exp
tupE' [a] = a
tupE' as = TupE as

tupP' :: [Pat] -> Pat
tupP' [a] = a
tupP' as = TupP as

-- Primitive operation names
opVar :: Show t => t -> Exp
opVar opc = VarE $ mkName $ "seq" ++ show opc

termExp :: T -> Exp
termExp (Comb1 _ opc a)     = app1 (opVar opc) (nodeExp a)
termExp (Comb2 _ opc a b)   = app2 (opVar opc) (nodeExp a) (nodeExp b)
termExp (Comb3 _ opc a b c) = app3 (opVar opc) (nodeExp a) (nodeExp b) (nodeExp c)
--termExp (Slice _ a b c)     = app3 (opVar "SLICE") (nodeExp a) (nodeExp b) (nodeExp c)
termExp e = error $ show e

app1 = AppE
app2 a b c     = app1 (app1 a b) c
app3 a b c d   = app1 (app2 a b c) d
app4 a b c d e = app1 (app3 a b c d) e




nodeExp :: N -> Exp          
nodeExp (Node _ n) = nodeNumExp n
nodeExp (Const (SInt _ v)) = LitE $ IntegerL $ fromIntegral v

nodeNumExp :: Int -> Exp          
nodeNumExp n = VarE $ nodeNumName n

nodeNumPat :: Int -> Pat          
nodeNumPat n = VarP $ nodeNumName n

nodeNumName :: Int -> Name
nodeNumName = mkName . nodeNumStr
nodeNumStr n = "r" ++ show n

