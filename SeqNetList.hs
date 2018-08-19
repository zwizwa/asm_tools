-- WIP: A simpler Term / Graph type.


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module SeqNetList where

import Seq
import Prelude hiding (foldr)
import SeqTerm(NodeNum)
import qualified SeqTerm
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map.Lazy(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Foldable
import Data.List
import Data.Graph


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
-- . After memory change, this is no longer sorted.  Create sorting routine?

data Form n =
  Input
  | Const   Int
  | Comb1   Op1 n
  | Comb2   Op2 n n
  | Comb3   Op3 n n n
  | Slice   n SSize NbBits
  | Memory  n n n n
  | Delay   n Int
  | Connect n
  deriving (Show, Functor, Foldable)



-- Converting between Term.Term and this makes sense only at the level
-- of a complete netlist.

convert ::
  [SeqTerm.Op NodeNum]
  -> [(NodeNum, SeqTerm.Term (SeqTerm.Op NodeNum))]
  -> NetList NodeNum

-- Ports need to be ordered, but the bindings are treated as a graph,
-- so we can pick an unordered representation.
data NetList n = NetList [n] (Bindings n)
type Bindings n = Map n (SSize, Form n)

type CompState = Int
type CompOut = Bindings' NodeNum
type Node n = (SSize, Form n)
type Bindings' n = [(n, Node n)]


newtype M t = M { unM :: WriterT CompOut (State CompState) t } deriving
    (Functor, Applicative, Monad, MonadState CompState, MonadWriter CompOut)

convert ports bindings = NetList ports' $ Map.fromList bindings'  where
  init = maximum $ map fst bindings
  ((ports', bindings'), _)  = runState (runWriterT $ unM mconv) init

  mconv = do
    mapM_ conv bindings
    mapM op ports

  -- Memory conversion: represent the memory as a combinatorial
  -- operation + a register at the output.  This requires a lookup for
  -- the type of the read data register.
  mem_rd_type :: Map NodeNum SType
  mem_rd_type = Map.fromList $ catMaybes $ map f bindings where
    f (rd, (SeqTerm.MemRd t (SeqTerm.MemNode mem))) = Just (mem, t)
    f _ = Nothing

  -- FIXME: the order of the Mem and Delay operations no longer
  -- respect the invariant.
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




-- Analysis.


-- Sort such that definition dominates use, which is needed for most
-- output code structures.  This is a topological sort from Data.Graph

data DAG n = DAG Graph (Vertex -> (Node n, n, [n])) (n -> Maybe Vertex)

sorted :: Ord n => DAG n -> Bindings' n
sorted (DAG graph vertex2node _) = reverse topSort' where
  topSort' = map unpack $ topSort graph where
  unpack v = (n, node) where (node, n, _) = vertex2node v
    
-- Convert to Data.Graph adjacency list representation.
-- Construct a DAG, so stop at Delay
-- FIXME: Or keep full cyclic graph but use Forest?

toDAG :: Ord n => Bindings n -> DAG n
toDAG bindings = DAG g v2n n2v where
  (g, v2n, n2v) = graphFromEdges edges'
  edges' = [(te,n, edges e) | (n,te@(t,e)) <- Map.assocs bindings]

  edges (Delay _ _) = []
  edges expr = toList expr



dependencies :: Ord n => DAG n -> n -> Set n
dependencies (DAG g v2n n2v) n = deps where
  -- Note that reachable contains the node itself, so remove.
  deps = Set.delete n reachable' 
  reachable' = Set.fromList $ map v2n' $ reachable g v 
  Just v = n2v n
  v2n' v = n' where (_, n', _) = v2n v
  



-- -- More misc tools that are probably better expressed in terms of DAG

-- dependsOn :: Ord n => DAG n -> n -> n -> Bool
-- dependsOn dag a b = b `elem` dependencies dag a

-- type Fanout n = Map n (Set n)

-- fanout :: forall n. Ord n => NetList n -> Fanout n
-- fanout (NetList ports bindings) = fo where
--   -- Traverse over all expression nodes, accumulating parent nodes.
--   fo = foldr acc_expr fo0 bindings'
--   acc_expr (n, (t, expr)) fo = foldr (acc_dep n) fo expr
--   acc_dep nout nin fo = Map.adjust (Set.insert nout) nin fo
--   fo0 = Map.fromList $ zip nodes $ cycle [Set.empty]
--   nodes = map fst bindings'
--   bindings' = Map.assocs bindings

-- -- Define a partition function to isolate Delay nodes
-- partition_delay = partition isDelay where
--   isDelay (_, Delay _ _) = True
--   isDelay _ = False

