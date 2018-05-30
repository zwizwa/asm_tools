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

data Term t
  = Comb1 Seq.Op1 t
  | Comb2 Seq.Op2 t t
  | Comb3 Seq.Op3 t t t
  | Delay t
  | Connect t
  | Input -- Externally driven node
  deriving (Show, Functor, Foldable)

-- Constants are not monadic values in Seq (tried that, and decided
-- it's too annoying), so the operand type Op has two clauses:
data Op t
  = Node  t         -- node reference
  | Const ConstVal  -- inlined constant
  deriving (Show, Functor, Foldable)


type NodeNum  = Int
type PortNum  = Int
type ConstVal = Int


-- It's convenient if the Drivers preserve order in the way they are
-- recorded to make sure definition dominates use.

newtype M t = M { unM :: WriterT (Bindings NodeNum) (State CompState) t } deriving
  (Functor, Applicative, Monad,
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

  -- undriven signal
  signal _  = fmap R makeNode
  stype _   = return $ Seq.SInt Nothing 0

  -- driven nodes
  constant (Seq.SInt _ v) = R $ Const v

  op1 o (R a) =
    fmap R $ driven $ Comb1 o a

  op2 o (R a) (R b) =
    fmap R $ driven $ Comb2 o a b

  op3 o (R a) (R b) (R c) =
    fmap R $ driven $ Comb3 o a b c

  -- register drive
  next (R (Node dst)) (R src) =
    driveNode dst $ Delay src

  -- Combinatorial drive is needed to support combinatorial module
  -- outputs, but otherwise not used in Seq.hs code.
  connect (R (Node dst)) (R src) =
    driveNode dst $ Connect src


-- For constants.
instance Num (R Seq.S) where
  fromInteger i = R $ Const $ fromInteger i
  -- Implement the rest just for constants.
  (+) = num2 (+)
  (*) = num2 (*)
  abs = num1 abs
  signum = num1 signum
  negate = num1 negate

num1 f (R (Const a)) =
  R $ Const $ f a
num2 f (R (Const a)) (R (Const b)) =
  R $ Const $ f a b



makeNode :: M (Op NodeNum)
makeNode = do
  n <- getNodeNum
  modifyNodeNum (+ 1)
  return $ Node n

driven c = do
  s@(Node n) <- makeNode
  driveNode n c
  return s


-- I/O ports direction is not known until it is driven, so start them
-- out as Input nodes ...
io :: Int -> M [R Seq.S]
io n = sequence $ [fmap R $ driven $ Input | _ <- [0..n-1]] 

-- ... and convert them to output here.  Other nodes cannot be driven
-- more than once.  Note: using fix, this error is avoided.
driveNode n c = do
  tell [(n, c)]
  
-- Compile to list of I/O ports and network map.
compile m = (map unR ports, cleanPorts nodes) where
  ((ports, nodes), nbNodes) = runState (runWriterT (unM m)) 0

-- Remove duplicates, keeping last, preserving order.
-- FIXME: generalize to functor output?
cleanPorts ports = ports' where
  ports' = reverse $ f Set.empty $ reverse ports
  f _ [] = []
  f s (n@(p,_):ns) =
    case p `Set.member` s of
      True -> f s ns
      False -> (n : f (p `Set.insert` s) ns)





