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
import Control.Monad.Identity

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

-- SYNTAX
data Opcode2 = Add | Mul deriving Show
data Term t where
  F    :: Float -> Term Float
  I    :: Int   -> Term Int
  Op2  :: Num t => Opcode2 -> Term t -> Term t -> Term t
  Lam  :: (Term t -> Term t') -> Term (t -> t')
  App  :: Term (t -> t') -> Term t -> Term t'
  Sig  :: Term t -> Term (t -> (t,  t')) -> Term t'
  Tup2 :: Term t -> Term t' -> Term (t,t')
  -- This is needed for compilation to represent open terms.  Should
  -- be hidden from library.
  Var  :: Int -> Term t


-- LIBRARY
square :: Num t => Term (t -> t)
square = Lam $ \x -> (Op2 Mul x x)



-- EVALUATOR

-- Compile to intermediate Ins language using Writer+State monad.
type E = Identity

eval2 f a b = do
  a' <- eval a
  b' <- eval b
  return $ f a' b'

eval :: forall t. Term t -> E t
eval (F f) = return f
eval (I i) = return i
eval (Op2 Add a b) = eval2 (+) a b
eval (Op2 Mul a b) = eval2 (*) a b
eval (App (Lam f) a) = eval $ f a

test_eval =
  runIdentity $
  eval $
  App (Lam (\x -> Op2 Add x x)) (F 1)
  


data Tagged = TFloat Float
            | TInt Int
            | TOp2 Opcode2 Tagged Tagged
            | TApp Tagged Tagged
            | TLam Tagged Tagged
            | TVar Int
            | TSig Tagged Tagged
            | TTup2 Tagged Tagged
            deriving Show



-- Compile to intermediate Ins language using Writer+State monad.
type C = WriterT [(Tagged,Tagged)] (State Int)

run_comp m = runState (runWriterT m) 0

-- Writer is not used.
-- State is used for variable generation.
newVar :: forall t. C (Term t)
newVar = do
  n <- get ; put $ n + 1
  return $ Var n
varNum (Var n) = n



-- COMPILER



comp :: forall t. Term t -> C Tagged
comp (F f) = return $ TFloat f
comp (I i) = return $ TInt i
comp (Var n) = return $ TVar n

comp (Tup2    a b) = comp2 TTup2      a b
comp (Op2 op2 a b) = comp2 (TOp2 op2) a b
comp (App     f a) = comp2 TApp       f a
  
-- Needs variable generation: new variable is stored with TLam form
-- and goes into f to get the substituted term.
comp (Lam f) = do
  v    <- newVar
  expr <- comp $ f v
  return $ TLam (TVar $ varNum v) expr

-- Hide a state machine (init value and update function).  The state
-- machine body needs to be compiled and saved somewhere.  Since it's
-- not needed anymore it can go to a writer.
comp (Sig i u) = do
  i' <- comp i
  u' <- comp u
  o  <- newVar
  let o' = TVar $ varNum o
  tell $ [(o', TSig i' u')]
  return o'

comp2 f a b = do
  a' <- comp a
  b' <- comp b
  return $ f a' b'

-- The writer has signal implementations, the val is the main expression.
c p = (sigs, val) where
  ((sigs,val),_state) = run_comp $ comp p
  
test_comp1 = c $ App (Lam (\x -> Op2 Add x x)) (F 1)

test_comp2 = c $ square

test_comp3 = c $ Sig (F 0) $ Lam (\x -> (Tup2 x x))
