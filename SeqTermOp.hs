{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module SeqTermOp where

import Seq
import SeqTerm(NodeNum)
import qualified SeqTerm
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Lazy as Map
import Data.Maybe

-- WIP: A simpler Term type.

-- This can serve as a decoupling type between SeqTerm conversiona and
-- postprocessing.  Essentially, remove redundancy and refactor.

-- Changes:
-- . Flatten double functor to one by adding constant nodes
-- . Restructure Memory as combinatorial Memory function + Delay
-- . Lift out typ
-- . Split STtype into bit size and value, drop value if not needed

-- Note that the Term/Op combo was originally introduced to represent
-- inline constants.

-- TODO:
-- . Change postproc code to use this representation
-- . Change SeqTerm to generate Form directly?  Possibly as Form . Op, then flatten.

data Form n =
  Const Int
  | Input
  | Comb1   Op1 n
  | Comb2   Op2 n n
  | Comb3   Op3 n n n
  | Slice   n SSize NbBits
  | Delay   n Int
  | Memory  n n n n
  | Connect n
  deriving (Show, Functor, Foldable)

-- Converting between the two makes sense only at the level of
-- bindings.  New node names need to be introduced for the constants.



type Binding n = (n, (SSize, Form n))
type CompState = Int
type CompOut = [Binding NodeNum]

newtype M t = M { unM :: WriterT CompOut (State CompState) t } deriving
    (Functor, Applicative, Monad, MonadState CompState, MonadWriter CompOut)


-- This is a lot of typing for such a simple transformation...  It
-- seems simplest to keep this just as an intermediate: fix the
-- original Term format after moving all syntax processing code to
-- this representation.  But first, everything else needs to be
-- translated to this representation, which is best done
-- incrementally.

convert ::
  [SeqTerm.Op NodeNum]
  -> [(NodeNum, SeqTerm.Term (SeqTerm.Op NodeNum))]
  -> ([NodeNum], [Binding NodeNum])
convert ports bindings = (ports', bindings')  where
  init = maximum $ map fst bindings
  ((ports', bindings'), _)  = runState (runWriterT $ unM mconv) init

  mconv = do
    mapM_ conv bindings
    mapM op ports

  -- Memory conversion: represent the memory as a combinatorial
  -- operation + a register at the output.  This requires a lookup for
  -- the type of the read data register.
  mem_rd_type :: Map.Map NodeNum SType
  mem_rd_type = Map.fromList $ catMaybes $ map f bindings where
    f (rd, (SeqTerm.MemRd t (SeqTerm.MemNode mem))) = Just (mem, t)
    f _ = Nothing

  -- FIXME: the order of the Mem and Delay operations no longer
  -- respect the invariant.  Fix this in post-procesing?

  conv :: (NodeNum, SeqTerm.Term (SeqTerm.Op NodeNum)) -> M ()

  conv (rd, (SeqTerm.MemRd t mem)) = do
    mem' <- op mem
    tell $ [(rd, (sz t, Delay mem' $ val t))]  -- FIXME: in practice this is undefined
  conv  (mem, (SeqTerm.MemWr (a,b,c,d))) = do
    let t = mem_rd_type Map.! mem
    a' <- op a ; b' <- op b ; c' <- op c ; d' <- op d
    tell $ [(mem, (sz t, Memory a' b' c' d'))]

  -- The rest is straightforward: lift out type while constants are extracted.
  conv (n, e) = do
    let (SInt sz _) = SeqTerm.termType e
    te' <- conv' e
    tell $ [(n, (sz, te'))]
  
  conv' (SeqTerm.Input t) = return $ Input
  
  conv' (SeqTerm.Delay t o)         = do [o']       <- ops [o];     return $ Delay o' $ val t
  conv' (SeqTerm.Connect _ o)       = do [o']       <- ops [o];     return $ Connect o'
  conv' (SeqTerm.Comb1 _ opc a)     = do [a']       <- ops [a];     return $ Comb1 opc a'
  conv' (SeqTerm.Comb2 _ opc a b)   = do [a',b']    <- ops [a,b];   return $ Comb2 opc a' b'
  conv' (SeqTerm.Comb3 _ opc a b c) = do [a',b',c'] <- ops [a,b,c]; return $ Comb3 opc a' b' c'
  conv' (SeqTerm.Slice _ o a b)     = do [o']       <- ops [o];     return $ Slice o' a b

  conv' _ = error $ "inernal error: already matched in conv"
  
  sz  (SInt x _) = x
  val (SInt _ x) = x
  
  ops = mapM op

  op (SeqTerm.MemNode n) = return n
  op (SeqTerm.Node t n) = return n
  op (SeqTerm.Const t) = do
    n <- newNode
    tell [(n, (sz t, Const $ val t))]
    return n

  newNode = do
    n <- get
    put $ n + 1
    return n




