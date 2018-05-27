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

module SeqNet where
import Seq
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Free
import Data.List
import Data.Map.Strict (Map, (!), empty, insert, insertWith, foldrWithKey)
import qualified Data.Map as Map
import qualified Data.Set as Set

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

-- FIXME: It's probably OK to use an expression language.
data Term t = Comb1 Op1 t
            | Comb2 Op2 t t
            | Comb3 Op3 t t t
            | Delay t
            | Connect t
            | Const ConstVal
            | Input
  deriving (Show, Foldable)

type Exp = Free Term Node

newtype Terms t = Terms [Term t] deriving Foldable

type Driver = Term Node

type Node     = Int
type PortNum  = Int
type ConstVal = Int


-- It's convenient if the Drivers preserve order in the way they are
-- recorded to make sure definition dominates use.

newtype M t = M { unM :: WriterT Bindings (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadWriter Bindings,
   MonadState CompState)

type Bindings = [(Node,Driver)]
type CompState = Node

-- Primitive state manipulations
appSignal = id
getSignal = get

data Signal = Sig Int deriving Show

-- Phantom representation wrapper
data R t = R { unR :: Signal }  

instance Seq M R where

  -- undriven signal
  signal _  = fmap R makeSignal
  stype _   = return $ SInt Nothing 0

  -- driven nodes
  constant (SInt _ v) =
    fmap R $ driven $ Const v

  op1 o (R (Sig a)) =
    fmap R $ driven $ Comb1 o a

  op2 o (R (Sig a)) (R (Sig b)) =
    fmap R $ driven $ Comb2 o a b

  op3 o (R (Sig a)) (R (Sig b)) (R (Sig c)) =
    fmap R $ driven $ Comb3 o a b c

  -- register drive
  next (R (Sig dst)) (R (Sig src)) =
    driveSignal dst $ Delay src

  -- Combinatorial drive is needed to support combinatorial module
  -- outputs, but otherwise not used in Seq.hs code.
  connect (R (Sig dst)) (R (Sig src)) =
    driveSignal dst $ Connect src

    

makeSignal = do
  n <- getSignal
  modify $ appSignal (+ 1)
  return $ Sig n

driven c = do
  s@(Sig n) <- makeSignal
  driveSignal n c
  return s


-- I/O ports direction is not known until it is driven, so start them
-- out as Input nodes ...
io :: Int -> M [R S]
io n = sequence $ [fmap R $ driven $ Input | _ <- [0..n-1]] 

-- ... and convert them to output here.  Other nodes cannot be driven
-- more than once.  Note: using fix, this error is avoided.
driveSignal n c = do
  -- FIXME: move this functionality to postproc
  -- Other bits need to
  -- let f c (Input _) = c
  --     f _ old_c = error $ "Signal driven twice: " ++ show (n,old_c,c)
  tell [(n, c)]
  
-- Compile to list of I/O ports and network map.
compile m = (map unpack ports, cleanPorts nodes) where
  ((ports, nodes), _) = runState (runWriterT (unM m)) 0
  unpack (R (Sig n)) = n

-- Remove duplicates, keeping last, preserving order.
cleanPorts ports = ports' where
  ports' = reverse $ f Set.empty $ reverse ports
  f _ [] = []
  f s (n@(p,_):ns) =
    case p `Set.member` s of
      True -> f s ns
      False -> (n : f (p `Set.insert` s) ns)


-- Inlining: everything except:
-- - Delay nodes
-- - Nodes with more than one user

nodeRefs nodes = refs where
  refs = foldr f Map.empty $ Terms $ map snd nodes
  f n = Map.insertWith (+) n 1

--nodeRefs nodes = 
--  foldr (:) [] $ Terms $ map snd nodes



-- First, a test.  Inline everything for a given node.


-- inline :: (Int -> Driver) -> Int -> Exp
-- inline ref = inl where
--   ref' = Pure . ref
--   inl n = fmap inl (ref' n)
  

-- test (ports, nodes) = map (inline (nodeMap !)) ports where
--   nodeMap = Map.fromList nodes
  

test (ports, nodes) = foldr (:) [] $ Terms $ map snd nodes

