-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
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
-- import Data.List
-- import Data.Maybe
-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Functor.Compose
-- import Data.Functor.Classes

-- WIP: A simpler Term type.

-- The Term/Op combo is necessary to represent inline constants.
-- However, for analysis it is really awkward to have the two nested
-- functors, so create a flattened version.

data Form n =
  Const     SType
  | Comb1   SType Seq.Op1 n
  | Comb2   SType Seq.Op2 n n
  | Comb3   SType Seq.Op3 n n n
  | Slice   SType n Seq.SSize Seq.NbBits
  | Delay   SType n
  | MemRd   SType n
  | MemWr   (n,n,n,n)
  | Connect SType n
  | Input   SType -- Externally driven node
  deriving (Show, Functor, Foldable)


-- Converting between the two makes sense only at the level of
-- bindings.  New node names need to be introduced for the constants.

type Binding n = (n, Form n)
type CompState = Int
type CompOut = [Binding NodeNum]

newtype M t = M { unM :: WriterT CompOut (State CompState) t } deriving
    (Functor, Applicative, Monad, MonadState CompState, MonadWriter CompOut)


convert :: [(NodeNum, SeqTerm.Term (SeqTerm.Op NodeNum))] -> [Binding NodeNum]
convert bindings = bindings'  where
  init = maximum $ map fst bindings
  (((), bindings'), _)  = runState (runWriterT $ unM mconv) init
  mconv :: M ()
  mconv = sequence_ $ map conv bindings

  conv :: (NodeNum, SeqTerm.Term (SeqTerm.Op NodeNum)) -> M ()
  conv (n, SeqTerm.Input t) = tell $ [(n, Input t)]
  conv (n, SeqTerm.Delay t o)     = do [o'] <- ops [o] ; tell $ [(n, Delay t o')]
  conv (n, SeqTerm.Connect t o)   = do [o'] <- ops [o] ; tell $ [(n, Connect t o')]
  conv (n, SeqTerm.Comb1 t opc a) = do [a'] <- ops [a] ; tell $ [(n, Comb1 t opc a')]
  conv _ = return ()

  ops = mapM op

  op (SeqTerm.MemNode n) = return n
  op (SeqTerm.Node t n) = return n
  op (SeqTerm.Const t) = do
    n <- newNode
    tell [(n, Const t)]
    return n

  newNode = do
    n <- get
    put $ n + 1
    return n
