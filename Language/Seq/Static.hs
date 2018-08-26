-- https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell

-- Trying to get a feel for DataKinds to work towards implementing
-- type-level handling of fixed signal sizes.  Note that this cannot
-- handle reset values other than zero.

-- EDIT: At this point this seems really too clumsy to be useful.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Seq.Static where

import qualified Language.Seq as Seq

-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html
import GHC.TypeLits
import Data.Word
import Foreign

newtype S (n :: Nat) a = S1 Seq.S

class Monad m => Seq m r | r -> m, m -> r where
  op2 :: r (S n) -> r (S n) -> m (r (S n))
  signal :: m (r (S n))

  -- signal = do
  --   let n = natVal
  --   s <- Seq.signal $ Seq.SInt (Just n) 0 
  --   return $ s

-- op2 o (S a) (S b) = do
--   c <- Seq.op2 o a b
--   return $ S c

