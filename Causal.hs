-- The idea here is to implement it upside-down, where sequences are
-- abstract, and the implementation is specialized using type families.

-- The idea is to keep the monadic Seq language as implementation, but
-- provide a (much) higher level interface in terms of applicative
-- functors.

{-# LANGUAGE TypeFamilies #-}

module Causal where

import Seq
import SeqTerm
import SeqEmu

-- Most generic: use standard Haskell type classes.

class Applicative f => Causal f where
  close :: (a -> (a, b)) -> a -> f b

instance Causal [] where
  close u s0 = f s0 where
    f s = (o : f s) where
      (s, o) = u s

-- This is straightforward
counter :: (Num t, Causal f) => t -> f t
counter = close (\s -> (s + 1, s))

-- But this needs a different approach.
-- integral :: (Num t, Causal f) => t -> f t -> f t
-- integral init ins =
  


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


