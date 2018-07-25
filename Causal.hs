-- The idea here is to implement it upside-down, where sequences are
-- abstract, and the implementation is specialized using type families.

-- The idea is to keep the monadic Seq language as implementation, but
-- provide a (much) higher level interface in terms of applicative
-- functors.

{-# LANGUAGE TypeFamilies #-}

module Causal where

import Data.Functor.Compose

import Seq
import SeqTerm
import SeqEmu

-- Most generic: use standard Haskell type classes.

-- Causal sequences.   How to express this as a law?
class Applicative f => Causal f where
  close :: (s -> i -> (s, o)) -> s -> f i -> f o

-- Instance for list is straightforward.  
instance Causal [] where
  close u s0 is  = f s0 is where
    f s (i:is) = (o : f s' is) where
      (s', o) = u s i

-- instance Causal (SeqTerm.M SeqTerm. where close = close'
-- instance Causal SeqEmu.M  where close = close'

-- close' :: Seq m r => (

--   close u s0 mi = do
--     closeReg $ \[s] -> do
--       let (s', o) = u (return s) mi
--       return ([s'], o)
    

integral :: (Num t, Causal f) => t -> f t -> f t
integral = close $ \s i -> (s + i, s)
counter s = integral s (pure 1)

-- Just use Compose.  No need for a specialzed data type.

-- This needs to go in each implementation.
-- instance Seq m r => Causal (SeqF m r) where
  




-- Allow Specialized instances.

data family Stream a
data instance Stream Int = StreamList [Int]

-- Not correct.  The idea is to see m (r S) as a stream.
-- data instance Stream S   = forall m r. Seq m r => StreamSeq (m (r S))

-- Maybe this only works for concrete types?
-- Move these instances into the appropriate files.
data instance Stream (SeqTerm.R S) = StreamTerm (SeqTerm.M (SeqTerm.R S))
data instance Stream (SeqEmu.R  S) = StreamEmu  (SeqEmu.M  (SeqEmu.R  S))

-- Options: add these as associated data types.
-- Or make it much more general and just specialize for the SeqTerm and SeqEmu variants.

-- Goal: reify integral

-- integral' :: Seq m r => m (r S) -> m (r S)
-- integral' = integral


-- instance Functor Stream where
  




-- Now what is the point of this?  To separate two things:
-- a) The scalar types for combinatorial operations
-- b) The sequential feedback

-- Now, does this need to be an associated type or not?  And
-- associated to what class exactly?


