-- https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell

-- Trying to get a feel for DataKinds to work towards implementing
-- type-level handling of fixed signal sizes.  Note that this cannot
-- handle reset values other than zero.

-- EDIT: At this point this seems really too clumsy to be useful.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module SeqStatic where

data Nat = Z | S Nat

infixl 6 :+

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

