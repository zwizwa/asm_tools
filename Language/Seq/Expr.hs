-- Expression language.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Seq.Expr where
import Language.Seq.Term(Term(..),Op(..))
import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq as Seq

--import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Data.List
--import Data.Maybe
import Data.Map.Strict (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Compose


-- MyHDL doesn't need to be in ANF, so provide a mechanism to
-- partially restore to an expression language for nodes that are not
-- shared.

-- The canonical way to do this uses the Free monad of the functor.
-- This took a day of typechecker fighting to finally understand, then
-- simplify.

type Term' = Compose Term Op  -- the Functor
type Expr n = Free Term' n    -- the Monad

-- These are horrible constructor names.  To read the code, note that:
-- a) Compose is just a wrapper to flatten Term and Op functors
-- b) Free essentially means "inline"



-- The main routine converts a flat dictionary to one that has
-- expressions inlined.
inlined :: forall n. Ord n => [(n, Term (Op n))] -> [(n, Expr n)]
inlined termBindings = [(n, exprDef n) | n <- keep] where

  keep = filter (not . inlinable) nodes
  exprDef n = (liftF $ Compose $ ref n)  -- unpack 1 level
              >>= unfold inline          -- plus inline

  inline :: n -> Either n (Term' n)
  inline n = case inlinable n of
               False -> Left n
               True  -> Right $ Compose $ ref n

  inlinable n = 
    case ref n of
      (Delay _ _) -> False      -- inlining Delay would create loops
      (MemWr _)   -> False
      (Input _)   -> False      -- keep external refernces as nodes
      _           -> 1 == rc n  -- rc > 1 would lead to code duplication
  
  -- Term dictionary.
  ref :: n -> Term (Op n)
  ref = ((Map.fromList termBindings) !)
  (nodes, terms) = unzip termBindings

  -- Reference count
  rc :: n -> Int
  rc n = Map.findWithDefault 0 n rcMap
  terms :: [Term (Op n)]
  rcMap = foldr count Map.empty $ Compose $ Compose $ terms
  count n = Map.insertWith (+) n 1




  
-- Show gives this error:
-- No instance for (Data.Functor.Classes.Show1 SeqTerm.Term)

-- So implement a printer.  Generic s-expression printer.  Serves as a
-- template for other generators using writer monad.

sexp' :: Show n => [(n, Expr n)] -> String
sexp' bindings =
  concat [concat [show n, " <- ", sexp e, "\n"] | (n, e) <- bindings]

-- Pile some formatting machinery on top of the Free monad.
newtype PrintExpr t = PrintExpr {
  runPrintExpr :: WriterT String (Reader IndentLevel) t
  }
  deriving (Functor, Applicative, Monad,
            MonadWriter String,
            MonadReader IndentLevel)
type IndentLevel = Int


sexp :: Show n => Expr n -> String
sexp e = str where
  ((), str) = runReader (runWriterT (runPrintExpr $ mSexp e)) 0


mSexp :: Show n => Expr n -> PrintExpr ()

mSexp (Pure n) = SeqTerm.tagged "NODE" [tell $ show n]
mSexp (Free (Compose e)) = SeqTerm.mTerm mSexp e


-- -- Indentation not used.
-- line str = do
--   n <- ask
--   sequence_ $ [tell "\t" | _ <- [1..n]]
--   tell $ str ++ "\n"

  

