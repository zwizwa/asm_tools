{-# LANGUAGE
ScopedTypeVariables,
GADTs
#-}

-- Attempt to solve the "monad hiding" problem by splitting the
-- problem into making an typed DSL as a GADT, combined with an
-- explicit interpreter.

module Language.DSP.Interp where


data Term t where
  Zero   :: Term Int
  Succ   :: Term Int -> Term Int
  Pred   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term t -> Term t -> Term t
  T      :: Term Bool
  F      :: Term Bool
            
eval :: forall t. Term t -> t
eval Zero = 0
eval (Succ a) = eval a + 1
eval (Pred a) = eval a - 1
eval T = True
eval F = False
eval (IsZero a) = eval a == 0
eval (If c t f) = if eval c then eval t else eval f

