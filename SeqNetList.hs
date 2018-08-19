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
import qualified SeqTerm
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map.Lazy(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Foldable
import qualified Data.List as List
import Data.Graph
import qualified Data.Array as Array


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
  [SeqTerm.Op Vertex]
  -> [(Vertex, SeqTerm.Term (SeqTerm.Op Vertex))]
  -> NetList Vertex

-- Ports need to be ordered, but the bindings are treated as a graph,
-- so we can pick an unordered representation.
data NetList n = NetList [n] (Bindings n)
type Bindings n = Map n (SSize, Form n)

type CompState = Int
type CompOut = Bindings' Vertex
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
  mem_rd_type :: Map Vertex SType
  mem_rd_type = Map.fromList $ catMaybes $ map f bindings where
    f (rd, (SeqTerm.MemRd t (SeqTerm.MemNode mem))) = Just (mem, t)
    f _ = Nothing

  -- FIXME: the order of the Mem and Delay operations no longer
  -- respect the invariant.
  conv :: (Vertex, SeqTerm.Term (SeqTerm.Op Vertex)) -> M ()

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

-- Convert to Data.Graph adjacency list representation.
-- To construct a DAG, cut off at Delay.
-- Note: see 'sorted' about what Delay means in this setting.


data DAG = DAG Graph (Bindings Vertex)
  

toDAG :: Bindings Vertex -> DAG
toDAG bindings = DAG graph bindings where
  nodes = Set.toList $ Map.keysSet bindings
  -- Impl: Vertex range should be reasonably dense for this to be
  -- efficient.  This seems to be the case.  If not, repack.
  graph = Array.array (minimum nodes, maximum nodes) init
  init = [(n, edges $ snd $ bindings Map.! n) | n <- nodes]
  edges (Delay _ _) = []
  edges expr = toList expr


transpose (DAG graph bindings) = DAG (transposeG graph) bindings


-- Sort such that definition dominates use, which is needed for most
-- output code structures.  This is a topological sort from Data.Graph

-- Because we've cut the connection made by Delay to obtain a DAG, the
-- Delay node in a sorted output represents a register output, and
-- should be thought of as input to the DAG only.  The argument node
-- of a Delay binding the contains the input to the register, and
-- needs to be considered as an output of the DAG.

-- E.g. in the following example, mode 151 is the output of the
-- register that will be used as an input in the following nodes,
-- while node 154 will only be computed later on and represents an
-- output of the DAG, input to the register.
--
-- (151,(Just 8,Delay 154 0),fromList [])

sorted :: DAG -> Bindings' Vertex
sorted (DAG graph bindings) = reverse topSort' where
  topSort' = map unpack $ topSort graph where
  unpack v = (v, bindings Map.! v)
    


-- Connectivity in both directions.
-- FIXME: n2v is wrong here.
fanout dag = deps (transpose dag)
deps (DAG g bs) n = g Array.! n
  
-- Closure of the above.
allFanout dag = allDeps (transpose dag)
allDeps :: DAG -> Vertex -> Set Vertex
allDeps (DAG g bs) v = deps where
  -- Note that reachable contains the node itself, so remove.
  deps = Set.delete v reachable' 
  reachable' = Set.fromList $ reachable g v


-- This can accomodate two styles of modules:
--
-- . Component modules: ports contain inputs and outputs, Input nodes
--   are input and Connect nodes set output nodes from internal nodes.
--
-- . Applicative I/O modules: ports are outputs, Input nodes are input

io :: Bindings' Vertex -> ([Vertex],[Vertex],[Vertex],[Vertex],[Vertex])
io bindings = (delays_in, delays_out, inputs, drives, rest) where
  [delays_in, delays_out, inputs, drives, rest] =
    map catMaybes $ List.transpose $ map select bindings
  select (n, (_, Delay n' _)) = [Just n', Just n,  Nothing, Nothing, Nothing]
  select (n, (_, Input))      = [Nothing, Nothing, Just n,  Nothing, Nothing]
  select (n, (_, Connect _))  = [Nothing, Nothing, Nothing, Just n,  Nothing ]
  select (n, _)               = [Nothing, Nothing, Nothing, Nothing, Just n]
  

-- NEXT: Pluck code from SeqExpr to create an inlined representation.
