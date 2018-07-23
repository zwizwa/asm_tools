-- Template Haskell rendering of a Seq program to remove
-- interpretative overhead.

-- Since Template Haskell is still a little unfamilar to me, I'm not
-- writing this as a tagless final interpreter, but as an explicit
-- compiler using SeqTerm

-- Interestingly, this doesn't need to be monadic.  The resulting
-- function can be constructed as pure  ((s,i)->(s,o))

{-# LANGUAGE TemplateHaskell #-}

module SeqTH where
import SeqTerm
import Seq
import qualified SeqLib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List


x_seqTH = m where
  c@(outputs, bindings) = compile $ do
    i <- SeqTerm.input SeqLib.bit
    -- c <- SeqLib.counter (SInt (Just 4) 0)
    o <- SeqLib.integral i
    return [o]
  m = do
    putStrLn "-- x_seqTH"
    print outputs
    sequence $ map print bindings
    -- expr <- runQ [| \f g x -> f (x*2 + 3) . g |]
    putStrLn $ pprint $ nLet outputs bindings

type N = Op NodeNum
type T = Term N
nLet :: [N] -> [(Int, T)] -> Exp
nLet outputs bindings = exp where
  exp = LamE [TupP [s, i]] $ LetE bs $ TupE [s',o]
  bs = [ValD (nVarP n) (NormalB (nExp e)) [] | (n, e) <- exprs]
  o = tupE' $ map nNodeE outputs

  -- Delay
  (delays,combs) = partition isDelay bindings
  isDelay (_,Delay _ _) = True ; isDelay _ = False
  s  = tupP' $ map (nVarP . fst) delays
  s' = tupE' [nNodeE n | (_, (Delay _ n)) <- delays]

  -- Inputs
  (inputs,exprs) = partition isInput combs
  isInput (_,Input _) = True ; isInput _ = False
  i = tupP' $ map (nVarP . fst) inputs



  
  
tupE' [a] = a
tupE' as = TupE as

tupP' [a] = a
tupP' as = TupP as

nExp :: T -> Exp
nExp (Comb1 _ opc a)   =   AppE (opVar opc) (nNodeE a)
nExp (Comb2 _ opc a b)   = AppE (AppE (opVar opc) (nNodeE a)) (nNodeE b)
nExp (Comb3 _ opc a b c) = AppE (AppE (AppE (opVar opc) (nNodeE a)) (nNodeE b)) (nNodeE c)
nExp e = error $ show e


opVar :: Show t => t -> Exp
opVar opc = VarE $ mkName $ "_" ++ show opc

nNodeE :: N -> Exp          
nNodeE (Node _ n) = nVarE n
nNodeE (Const (SInt _ v)) = LitE $ IntegerL $ fromIntegral v

nVarE :: Int -> Exp          
nVarE n = VarE $ varName n

nVarP :: Int -> Pat          
nVarP n = VarP $ varName n

varName :: Int -> Name
varName = mkName . varStr
varStr n = "r" ++ show n

