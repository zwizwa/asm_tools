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



x_seqTH = m where
  c@(outputs, bindings) = compile $ do
    t <- SeqLib.counter (SInt (Just 4) 0)
    return [t]
  m = do
    putStrLn "-- x_seqTH"
    print outputs
    sequence $ map print bindings
    -- expr <- runQ [| \f g x -> f (x*2 + 3) . g |]
    putStrLn $ pprint $ nDo outputs bindings

type N = Op NodeNum
type T = Term N
nDo :: [N] -> [(Int, T)] -> Exp
nDo outputs bindings = exp where
  exp = DoE $ bs ++ [NoBindS $ AppE _return os]
  bs = [nBind n e | (n, e) <- bindings]
  os = TupE $ map nVarE' outputs

_return :: Exp
_return = VarE $ mkName "return"
_next :: Exp
_next   = VarE $ mkName "next"

nBind :: Int -> T -> Stmt
nBind n (Delay _ n') =
  NoBindS $ AppE (AppE _next (LitE $ StringL $ varStr n)) (nVarE' n')
nBind n e =
  BindS (nVarP n) (nExp e)

nExp :: T -> Exp
nExp (Comb1 _ opc a)   = AppE (nOp opc) (nVarE' a)
nExp (Comb2 _ opc a b) = AppE (AppE (nOp opc) (nVarE' a)) (nVarE' b)
nExp e = error $ show e

nOp :: Show t => t -> Exp
nOp opc = VarE $ mkName $ "_" ++ show opc

nVarE' :: N -> Exp          
nVarE' (Node _ n) = nVarE n
nVarE' (Const (SInt _ v)) = LitE $ IntegerL $ fromIntegral v


nVarE :: Int -> Exp          
nVarE n = VarE $ varName n

nVarP n = VarP $ varName n

varName :: Int -> Name
varName = mkName . varStr
varStr n = "r" ++ show n

