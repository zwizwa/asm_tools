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

class Applicative f => Causal f where
  close :: (a -> (a, b)) -> a -> f b

instance Causal [] where
  close u s0 = f s0 where
    f s = (o : f s) where
      (s, o) = u s

integral :: (Num t, Causal f) => t -> f t
integral = close (\s -> (s + 1, s))


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

