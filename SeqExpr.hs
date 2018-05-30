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
import SeqTerm
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
  -- Dictionary.  Keep this unwrapped.
  ref :: n -> Term (Op n)
  ref = ((Map.fromList termBindings) !)

  -- Code will operate on nodes (NodeNum).  This renders to Term'
  -- What to keep. Need to preserve order, so use a list.
  keep = map fst termBindings  -- FIXME
  exprBindings = [(n, exprDef n) | n <- keep]  -- FIXME: cutuff 1? (*)
  exprDef n = inlineNode ref n

-- (*) Basically, it's guaranteed that there is at least one level, so
-- fmap over the original term with Free wrappers.



inlineNode :: (n -> Term (Op n)) -> n -> Expr n
inlineNode ref n = Expr $ e where
  e = unfold inl n
  inl n = em where
    t = ref n
    em = case t of
      (Delay _) -> Left n
      _ -> Right $ Compose t
      
  

-- There was an error about Show1 I didn't understand:
-- No instance for (Data.Functor.Classes.Show1 SeqTerm.Term)

-- So I'm taking the detour to implement a printer explicitly.

-- Generic s-expression printer.  Serves as a template for other
-- generators.

sexp' :: Show n => [(n, Expr n)] -> String
sexp' bindings =
  concat [concat [show n, "<-", sexp e, "\n"] | (n, e) <- bindings]

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
  Expr (Pure ((), str)) = runReaderT (runWriterT (unM' $ mSexp' e)) 0


-- Keep this wrapper: easier to express the types.
mSexp' :: Show n => Expr n -> M' ()
mSexp' (Expr e) = mSexp e

mSexp (Pure n) = tagged "node" [tell $ show n]
mSexp (Free (Compose e)) = mTerm e

mTerm Input           = tagged "input" []
mTerm (Delay a)       = tagged "delay" [mOp a]
mTerm (Connect a)     = tagged "connect" [mOp a]
mTerm (Comb1 o a)     = tagged (show o) [mOp a]
mTerm (Comb2 o a b)   = tagged (show o) [mOp a, mOp b]
mTerm (Comb3 o a b c) = tagged (show o) [mOp a, mOp b, mOp c]

mOp (Const v) = tagged "const" [ tell $ show v]
mOp (Node n)  = mSexp n

tagged tag ms = do
  tell "("
  tell tag
  sequence_ $ map ((tell " ") >>) ms
  tell ")"




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


