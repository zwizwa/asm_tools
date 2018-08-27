-- WIP: A simpler Term / Graph type.


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.Seq.NetList where

import Language.Seq
import qualified Language.Seq.Term as SeqTerm

import Prelude hiding (foldr)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map.Lazy(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Foldable
import Data.List
import Control.Monad.Free
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

data TypedForm n = TypedForm { typedFormType :: SSize, typedFormForm :: Form n }
  deriving (Show, Functor, Foldable)


-- Converting between Term.Term and this makes sense only at the level
-- of a complete netlist.
convert ::
  ([SeqTerm.Op Vertex],
   [(Vertex, SeqTerm.Term (SeqTerm.Op Vertex))],
   [(SeqTerm.Op Vertex, String)])
  -> NetList Vertex

-- Ports need to be ordered, but the bindings are treated as a graph,
-- so we can pick an unordered representation.
data NetList n = NetList [n] (BindMap n) [(n,String)]
type BindMap n = Map n (TypedForm n)

type CompState = Int
type CompOut = BindList Vertex
type BindList n = [(n, TypedForm n)]


newtype M t = M { unM :: WriterT CompOut (State CompState) t } deriving
    (Functor, Applicative, Monad, MonadState CompState, MonadWriter CompOut)

convert (ports, bindings, probes) = NetList ports' (Map.fromList bindings') probes'  where
  init = maximum $ map fst bindings
  ((ports', bindings'), _)  = runState (runWriterT $ unM mconv) init

  probes' :: [(Vertex, String)]
  probes' = catMaybes $ map probeType probes
  probeType (op, name) = do
    n <- SeqTerm.opNode op
    return (n, name)

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
    tell $ [(rd, TypedForm (sz t) (Delay mem' $ val t))]  -- FIXME: in practice this is undefined
  conv  (mem, (SeqTerm.MemWr (a,b,c,d))) = do
    let t = mem_rd_type Map.! mem
    a' <- op a ; b' <- op b ; c' <- op c ; d' <- op d
    tell $ [(mem, TypedForm (sz t) (Memory a' b' c' d'))]

  -- The rest is straightforward: lift out type while constants are extracted.
  conv (n, e) = do
    let (SInt sz _) = SeqTerm.termType e
    e' <- conv' e
    tell $ [(n, TypedForm sz e')]
  
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
    tell [(n, TypedForm (sz t) (Const $ val t))]
    return n

  newNode = do
    n <- get
    put $ n + 1
    return n




-- Analysis.

-- Annotate bindings with a Data.Graph adjacency list representation.
-- Keep two annotations:
-- . full cyclic graph
-- . combinatorial a-cyclic graph, cut off at Delay

data DG = DG { dcg :: Graph, dag :: Graph, bindMap :: BindMap Vertex }

toDG :: BindMap Vertex -> DG
toDG bindings = DG (toGraph toList) (toGraph toList') bindings where

  nodes = Set.toList $ Map.keysSet bindings

  toList' (Delay _ _) = []
  toList' expr = toList expr

  toGraph :: (Form Vertex -> [Vertex]) -> Graph
  toGraph edges = graph where
    -- Impl: Vertex range should be reasonably dense for this to be
    -- efficient.  This seems to be the case.  If not, repack.
    graph = Array.array (minimum nodes, maximum nodes) init
    init = [(n, edges $ typedFormForm $ bindings Map.! n) | n <- nodes]



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

sorted :: DG -> BindList Vertex
sorted (DG _ dag bindings) = reverse topSort' where
  topSort' = map unpack $ topSort dag where
  unpack v = (v, bindings Map.! v)
    

-- Connectivity in both directions.
deps (DG dcg _ bs) n = dcg Array.! n
fanout (DG dcg _ bs) n = (transposeG dcg) Array.! n

refcount dg = length . (fanout dg)


-- -- FIXME: these are not really necessary
-- -- Closure of the above.
-- allFanout (DG _ g bs) = allDeps (DAG (transposeG g) bs)
-- allDeps :: DG -> Vertex -> Set Vertex
-- allDeps (DG _ g bs) v = deps where
--   -- Note that reachable contains the node itself, so remove.
--   deps = Set.delete v reachable'
--   reachable' = Set.fromList $ reachable g v


-- This can accomodate two styles of modules:
--
-- . Component modules: ports contain inputs and outputs, Input nodes
--   are input and Connect nodes set output nodes from internal nodes.
--
-- . Applicative I/O modules: ports are outputs, Input nodes are input

io :: BindList Vertex -> ([Vertex],[Vertex],[Vertex],[Vertex],[Vertex])
io bindings = (delays_in, delays_out, inputs, drives, rest) where
  [delays_in, delays_out, inputs, drives, rest] =
    map catMaybes $ List.transpose $ map select bindings
  select (n, (TypedForm _ (Delay n' _))) = [Just n', Just n,  Nothing, Nothing, Nothing]
  select (n, (TypedForm _ Input))        = [Nothing, Nothing, Just n,  Nothing, Nothing]
  select (n, (TypedForm _ (Connect _)))  = [Nothing, Nothing, Nothing, Just n,  Nothing ]
  select (n, _)                          = [Nothing, Nothing, Nothing, Nothing, Just n]
  

-- Convert a flat dictionary to one that has expressions inlined where
-- possible.  Note that we get rid of the types of the intermediate
-- nodes.  This should be fine: the point of this is to generate
-- target code that supports expressions, which will not have type
-- annotations for intermediate nodes either.

type TypedExpr' n = Free TypedForm n
newtype TypedExpr n = TypedExpr (TypedExpr' n)
-- 'deriving Show' doesn't work on 8.4.3. due to missing Show1 instance.
-- I have no time to figure this out, so see the explicit Show instance below.


inlined :: DG -> [(Vertex, TypedExpr Vertex)]
inlined dg@(DG _ _ bindings) = [(n, TypedExpr $ exprDef n) | n <- keep] where

  ref :: Vertex -> TypedForm Vertex
  ref n = bindings Map.! n

  keep :: [Vertex]
  keep = filter (not . inlinable) $ map fst $ sorted dg

  exprDef :: Vertex -> TypedExpr' Vertex
  exprDef n = (liftF $ ref n)    -- unpack at least root expression
              >>= unfold inline  -- plus inline

  -- The Pure/Free unfold decision is represented as Either
  inline :: Vertex -> Either Vertex (TypedForm Vertex)
  inline n = case inlinable n of
               False -> Left n
               True  -> Right $ ref n

  inlinable :: Vertex -> Bool
  inlinable n = 
    case form of
      (Delay _ _) ->      False -- inlining Delay would create loops
      (Memory _ _ _ _) -> False -- keep this separate for easy code gen
      (Input)          -> False -- keep external refernces as nodes
      _ -> 1 == refcount dg n   -- rc > 1 would lead to code duplication
    where (TypedForm _ form) = ref n
  


-- S-expression printer

se lst = "(" ++ intercalate " " lst ++ ")"

showF Input = "INPUT"
showF (Const n) = show n
showF (Comb1 o a) = se [show o, show a]
showF (Comb2 o a b) = se $ [show o] ++ map show [a,b]
showF (Comb3 o a b c) = se $ [show o] ++ map show [a,b,c]
showF (Slice n a b) = se $ ["SLICE", show n, showSZ a, show b]
showF (Memory a b c d) = se $ ["MEMORY"] ++ map show [a,b,c,d]
showF (Delay n i) = se $ ["DELAY", show n, show i]
showF (Connect n) = se $ ["CONNECT", show n]
  
showTF (TypedForm sz f) = show f ++ "::" ++ showSZ sz where

showTE (Pure n) = show n
showTE (Free f) = show f

showSZ Nothing = "?"
showSZ (Just n) = show n

instance Show n => Show (TypedExpr n) where
  show (TypedExpr te) = showTE te


compileTerm = convert . SeqTerm.compileTerm


