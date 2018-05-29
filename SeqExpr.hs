-- Expression language.

-- MyHDL doesn't need to be in ANF, so provide a mechanism to
-- partially restore to an expression language.

-- The canonical way to do this uses a the Free monad of the functor.


-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module SeqExpr where
import SeqNet
import qualified Seq as Seq
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



type Term' = Compose Term Op
newtype Expr n = Expr {unExpr :: Free Term' n}
  deriving (Functor, Applicative, Monad)

-- The main routine converts a flat dictionary to one that has
-- expressions inlined.
inlined :: forall n. Ord n => [(n, Term (Op n))] -> [(n, Expr n)]
inlined termBindings = exprBindings where
  -- Wrapper used throughout
  termBindings' = map (\(n,t) -> (n, Compose t)) termBindings  -- wrap
  -- Code will operate on nodes (NodeNum).  This renders to Term'
  ref :: n -> Term' n
  ref = ((Map.fromList termBindings') !)
  -- What to keep. Need to preserve order, so use a list.
  keep = map fst termBindings  -- FIXME
  exprBindings = [(n, inlinedNode ref n) | n <- keep]

inlinedNode :: (n -> Term' n) -> n -> Expr n
inlinedNode ref n = Expr $ unfold inl n where
  inl n = em where
    t = ref n
    em = case t of
      (Compose (Delay _)) -> Left n
      _ -> Right t
      
  

-- There was an error about Show1 I didn't understand:
-- No instance for (Data.Functor.Classes.Show1 SeqNet.Term)

-- So I'm taking the detour to implement a printer explicitly.

-- Generic s-expression printer.  Serves as a template for other
-- generators.

sexp' :: Show n => [(n, Expr n)] -> String
sexp' bindings =
  concat [(list [show n, sexp e]) ++ "\n" | (n, e) <- bindings]

parens s = "(" ++ s ++ ")"
spaced = intercalate " "
list l = parens $ spaced l

-- This is "the other" monad.  Clean this up.
-- It tags some formatting machinery to the Free monad.
newtype M' t = M' { unM' :: WriterT String (ReaderT IndentLevel Expr) t } deriving
  (Functor, Applicative, Monad,
   MonadWriter String,
   MonadReader IndentLevel)
type IndentLevel = Int

-- FIXME: This took some type checker fight.  The idea is simpler, but
-- the final solution is only obvious in retrospect.
sexp :: Show n => Expr n -> String
sexp e = str where
  m = mSexp e
  Expr (Pure ((), str)) = runReaderT (runWriterT (unM' m)) 0

node n = "n" ++ show n

mSexp :: Show n => Expr n -> M' ()
mSexp (Expr (Pure n)) = tell $ node n
mSexp (Expr (Free e)) = tell $ "<<dummy>>"

line str = do
  n <- ask
  sequence_ $ [tell "\t" | _ <- [1..n]]
  tell $ str ++ "\n"

  
-- mSexp v = do
--   -- tell "<dummy>"
--   tell $ show v
--   return ()



-- -- Converting from "basic template" to "recursive type" is exactly
-- -- what the Free monad does:
-- newtype Term' n = Term (Op n)
-- type Expr n  = Free Term' n

-- -- We can inline everything except:
-- -- a) Delay nodes would create cycles
-- -- b) Input nodes are external
-- -- c) Nodes with Fanout>1 would create duplicate code

-- inlinable :: Ord n => Map n (Term (Op n)) -> n -> Bool
-- inlinable terms = pred where
--   rc = refcount $ Map.elems terms
--   pred n =
--     case terms ! n of
--       (Delay _) -> False
--       Input     -> False
--       _         -> 1 == rc n

-- -- Utility wrapper: folds over all nodes in a list of terms.
-- newtype Terms n = Terms [Term (Op n)] deriving Foldable

-- refcount :: Ord n => [Term (Op n)] -> (n -> Int)
-- refcount terms n = Map.findWithDefault 0 n map where
--   map = foldr count Map.empty $ Terms $ terms
--   count n = Map.insertWith (+) n 1

-- -- Inline node based on predicate.
-- inlineP :: (n -> Bool) -> (n -> Term (Op n)) -> n -> Expr n
-- inlineP p ref = inl where
--   inl (Term' (Node n)) = case (p n) of
--     -- liftM :: Term n -> Expr n
--     True  -> liftF (ref n) >>= inl
--     False -> return n

-- -- Bindings as list, to keep order for code gen.
-- inlined :: Ord n => [(n, Term' n)] -> [(n, Expr n)]
-- inlined bindings = map outBinding keep where

--   keep = filter (not . inlinable') nodes
--   outBinding n = (n, Free $ fmap inline $ ref n)
  
--   inline = inlineP inlinable' ref
--   inlinable' = inlinable bindings'
--   ref = (bindings' !)
--   bindings' = Map.fromList bindings
--   nodes = map fst bindings




-- -- Generic s-expression printer.  Serves as a template for other
-- -- generators.

-- sexp' :: Show n => [(n, Expr (Op n))] -> String
-- sexp' bindings =
--   concat [(list [show n, sexp e]) ++ "\n" | (n, e) <- bindings]

-- parens s = "(" ++ s ++ ")"
-- spaced = intercalate " "
-- list l = parens $ spaced l

-- sexp :: Show n => Expr (Op n) -> String
-- sexp = show



