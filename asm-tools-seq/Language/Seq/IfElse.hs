-- FIXME: See rtl.txt, comments on Jan DeCaluwe's blog posts.

-- Basically, it seems simpler to do use custom RTL generators as
-- compared to Verilog's built-in imperative bevioral code (that
-- compiles to combinatorial update networks).

-- Extension of SeqExpr that can accomodate nested imperative
-- conditionals.  The idea is to make generated MyHDL code more
-- readable, and ensure that MyHDL can produce case statements.



{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Seq.IfElse where

import Language.Seq.Term
import Language.Seq(Seq,S)
import qualified Language.Seq as Seq
import qualified Language.Seq.Term as SeqTerm
-- import Language.Seq.Test.Tools

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Compose

-- How to tackle this?
-- . create test-seq x_seqIfElse that prints SeqExpr form of async receiver
-- . find a way to modify the SeqExpr data structure to include this nesting


-- It seems that the main change is to convert the single binding to a
-- multiple binding, then hoist the conditional out of the list of
-- bindings.

-- It seems clear that doing this on SeqExpr is not a good idea.
-- Perform the operation on SeqTerm first, by recursively creating
-- subprograms.  So define what a subprogram is first.  It is a set of
-- bindings and a set of outputs.

-- This might be a good place to use the nested tree vs path
-- correspondence.  Which is exactly case vs. nested if!  Paths might
-- be easy to calculate as a list of conditionals or their negation.
-- Still, a tree rep would be useful.

-- First, make a better printer for STerm.


x :: SeqTerm.M [SeqTerm.R S] -> IO ()
x term = do
  let (ports, bindings, _) = compileTerm term
      --bindings' = inlined bindings
      --bindings' :: [(Int, Expr Int)]
  putStrLn "  term"
  print  ports
  -- printL bindings
  putStr $ sexp' bindings
  -- putStrLn "  expr"
  -- putStr $ sexp' bindings'
  

-- FIXME: Look at Chalmers Lava paper, it has a "behavioral" language
-- embedded that is then compiled to a "full tree" register update.
