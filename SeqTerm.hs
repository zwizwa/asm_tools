-- Network rendering of a Seq program 
-- Used as a base for MyHDL rendering.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module SeqTerm where
import Seq(SType(..),Env,initEnv)
import qualified Seq as Seq
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Data.List
import Data.Maybe
import Data.Map.Strict (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Compose

-- Seq does not provide a way to create modules with abstract I/O.
-- Specific constructs in this module are used to compose basic Seq
-- operations into something that can be rendered as a MyHDL module.

-- Seq is not a full HDL.  In a typical setting, a circuit will
-- contain pure combinatorial logic, and possibly different clock
-- domains.  Seq cannot simulate those, so some simulation needs to be
-- handed off to MyHDL.  This is the main reason to provide module
-- output: to be included in a larger discrete event simulation.

-- The basic idea is to:
-- 1) Use Seq to design sequential circuits
-- 2) Use MyHDL to perform simulation and HDL export
-- 3) Use Verilog/VHDL compiler to synthesize logic


data Term n
  = Comb1   SType Seq.Op1 n
  | Comb2   SType Seq.Op2 n n
  | Comb3   SType Seq.Op3 n n n
  | Slice   SType n Seq.SSize Seq.NbBits
  | Delay   SType n
  | MemRd   SType n
  | MemWr   (n,n,n,n)
  | Connect SType n
  | Input   SType -- Externally driven node
  deriving (Show, Functor, Foldable)

-- Constants are not monadic values in Seq (tried that, and decided
-- it's too annoying), so the operand type Op has two clauses:
data Op n
  = Node    SType n   -- node reference (*)
  | Const   SType     -- inlined constant
  | MemNode n
  deriving (Show, Functor, Foldable)


-- (*) Type-annotated because due to use of writer monad, there is no
-- access to the dictionary to retreive node type from a plain
-- reference.  This introduces copying and some places where there are
-- representable invalid states needing asserts, plus the awkward type
-- selectors here.  How to fix that?
termType (Comb1 t _ _) = t
termType (Comb2 t _ _ _) = t
termType (Comb3 t _ _ _ _) = t
termType (Slice t _ _ _) = t
termType (Delay t _) = t
termType (Connect t _) = t
termType (Input t) = t

opType (Const t) = t
opType (Node t _) = t


type NodeNum  = Int
type PortNum  = Int
type ConstVal = Int


-- It's convenient if the Drivers preserve order in the way they are
-- recorded to make sure definition dominates use.

newtype M t = M { unM ::
                    ReaderT (Env R)
                    (WriterT (Bindings NodeNum)
                     (State CompState))
                    t
                } deriving
  (Functor, Applicative, Monad,
   MonadReader (Env R),
   MonadWriter (Bindings NodeNum),
   MonadState CompState)

type Bindings n = [(n, Term (Op n))]
type CompState = NodeNum

-- Primitive state manipulations
modifyNodeNum :: (NodeNum -> NodeNum) -> M ()
modifyNodeNum = modify
getNodeNum :: M NodeNum
getNodeNum = get


-- Phantom representation wrapper
data R t = R { unR :: Op NodeNum }  

instance Seq.Seq M R where

  constant t = R $ Const t

  -- undriven signal
  signal typ   = fmap R $ makeNode typ
  stype (R op) = return $ opType op

  -- driven nodes

  -- FIXME: use Seq.hs SType functions
  -- This does not perform the necessary checks.
  
  op1 o (R a) =
    fmap R $ driven $ Comb1 (combTypes [a]) o a

  op2 o@Seq.EQU (R a) (R b) =
    fmap R $ driven $ Comb2 (SInt (Just 1) 0) o a b
    
  op2 o (R a) (R b) =
    fmap R $ driven $ Comb2 (combTypes [a,b]) o a b

  op3 o@Seq.IF (R a) (R b) (R c) =
    fmap R $ driven $ Comb3 (combTypes [b,c]) o a b c

  slice (R a) b c =
    fmap R $ driven $ Slice (combTypes [a]) a b c

  -- Combinatorial drive is needed to support combinatorial module
  -- outputs, but otherwise not used in Seq.hs code.
  connect = bind Connect

  -- register drive
  update   = bind Delay

  -- see comments in Seq.erl
  getEnv = ask
  withEnv = local

  memory td = do
    mem  <- fmap MemNode makeNodeNum
    rData <- driven $ MemRd td mem
    return (R rData, R mem)

  updateMemory (R (MemNode n)) (R wEn, R wAddr, R wData, R rAddr) = do
    driveNode n $ MemWr (wEn, wAddr, wData, rAddr)

bind cons (R (Node t' dst)) (R src) = do
    let t = opType src
    -- case t == t' of
    --   False -> error $ "bind: different node types: " ++ show (t',t)
    --   True  -> driveNode dst $ cons t src
    driveNode dst $ cons t src


-- For constants.
instance Num (R Seq.S) where
  fromInteger i = R $ Const $ SInt Nothing $ fromInteger i
  -- Implement the rest just for constants.
  (+) = num2 (+)
  (*) = num2 (*)
  abs = num1 abs
  signum = num1 signum
  negate = num1 negate

num1 f (R (Const (SInt s a))) =
  R $ Const $ SInt s $ f a
num2 f (R (Const (SInt sa a))) (R (Const (SInt sb b))) =
  R $ Const $ SInt (mergeSize [sa,sb]) $ f a b


-- Note that this only works for combinatorial results, which do not
-- have an initial value, but do have a bit width.  Another indication
-- of data structures not being encoded well.
combTypes :: [Op NodeNum] -> SType
combTypes ns = SInt size 0 where
  sizes = [s | (SInt s _) <- map opType ns]
  size = mergeSize sizes


mergeSize :: [Maybe Seq.NbBits] -> Maybe Seq.NbBits
mergeSize [] = error "mergeSize internal error"
mergeSize [t] = t
mergeSize (ta:tb:ts) = mergeSize ((f ta tb):ts) where
  f sz Nothing = sz
  f Nothing sz = sz
  f (Just a) (Just b) = Just $ max a b


makeNode :: SType -> M (Op NodeNum)
makeNode t = fmap (Node t) makeNodeNum

makeNodeNum :: M NodeNum
makeNodeNum = do
  n <- getNodeNum
  modifyNodeNum (+ 1)
  return n
  

driven c = do
  s@(Node _ n) <- makeNode $ termType c
  driveNode n c
  return s


-- I/O ports direction is not known until it is driven, so start them
-- out as Input nodes ...
io :: [SType] -> M [R Seq.S]
io ts = sequence $ map input ts

input t = fmap R $ driven $ Input t

-- ... and convert them to output here.  Other nodes cannot be driven
-- more than once.  Note: using fix, this error is avoided.
driveNode n c = do
  tell [(n, c)]
  
-- Compile to list of I/O ports and network map.
compile :: M [R t] -> ([Op NodeNum], [(NodeNum, Term (Op NodeNum))])
compile m = (map unR ports, cleanPorts nodes) where
  ((ports, nodes), nbNodes) =
    runState (runWriterT (runReaderT (unM m) initEnv)) 0

-- Remove duplicates, keeping last, preserving order.
-- FIXME: generalize to functor output?
cleanPorts ports = ports' where
  ports' = reverse $ f Set.empty $ reverse ports
  f _ [] = []
  f s (n@(p,_):ns) =
    case p `Set.member` s of
      True -> f s ns
      False -> (n : f (p `Set.insert` s) ns)

