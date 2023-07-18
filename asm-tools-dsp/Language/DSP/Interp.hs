{-# LANGUAGE
ScopedTypeVariables,
GADTs
#-}

-- Attempt to solve the "monad hiding" problem by splitting the
-- problem into making an typed DSL as a GADT, combined with an
-- explicit interpreter.

module Language.DSP.Interp where

import Control.Monad.State
import Control.Monad.Writer

-- Example from http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf

-- data Term t where
--   Zero   :: Term Int
--   Succ   :: Term Int -> Term Int
--   Pred   :: Term Int -> Term Int
--   IsZero :: Term Int -> Term Bool
--   If     :: Term Bool -> Term t -> Term t -> Term t
--   T      :: Term Bool
--   F      :: Term Bool
            
-- eval :: forall t. Term t -> t
-- eval Zero = 0
-- eval (Succ a) = eval a + 1
-- eval (Pred a) = eval a - 1
-- eval T = True
-- eval F = False
-- eval (IsZero a) = eval a == 0
-- eval (If c t f) = if eval c then eval t else eval f


-- To get started, create a very simple language and implement a
-- Haskell evaluator and a compiler to tagged syntax.

data Opcode2 = Add | Mul deriving Show

data Term t where
  F    :: Float -> Term Float
  I    :: Int   -> Term Int
  Op2  :: Num t => Opcode2 -> Term t -> Term t -> Term t
  Lam  :: (Term t -> Term t') -> Term (t -> t')
  App  :: Term (t -> t') -> Term t -> Term t'
  -- This is needed for compilation, to probe functions.
  Var  :: Int -> Term t

eval :: forall t. Term t -> t
eval (F f) = f
eval (I i) = i
eval (Op2 Add a b) = (eval a) + (eval b)
eval (Op2 Mul a b) = (eval a) * (eval b)
eval (App (Lam f) a) = eval (f a)

test_eval =
  eval $ App (Lam (\x -> Op2 Add x x)) (F 1)
  


data Tagged = TFloat Float
            | TInt Int
            | TOp2 Opcode2 Tagged Tagged
            | TApp Tagged Tagged
            | TLam Tagged Tagged
            | TVar Int
            deriving Show



-- Compile to intermediate Ins language using Writer+State monad.
type M = WriterT String (State Int)

run_comp m = runState (runWriterT m) 0

-- Writer is not used.
-- State is used for variable generation.
newVar :: forall t. M (Term t)
newVar = do
  n <- get ; put $ n + 1
  return $ Var n
varNum (Var n) = n




comp :: forall t. Term t -> M Tagged
comp (F f) = return $ TFloat f
comp (I i) = return $ TInt i
comp (Var n) = return $ TVar n

comp (Op2 op2 a b) = do
  a' <- comp a
  b' <- comp b
  return $ TOp2 op2 a' b'
  
comp (App f a) = do
  f' <- comp f
  a' <- comp a
  return $ TApp f' a'
  
-- Needs variable generation: new variable is stored with TLam form
-- and goes into f to get the substituted term.
comp (Lam f) = do
  v    <- newVar
  expr <- comp $ f v
  return $ TLam (TVar $ varNum v) expr





test_comp =
  run_comp $
  comp $
  App (Lam (\x -> Op2 Add x x)) (F 1)
