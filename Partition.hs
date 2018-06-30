
-- Arrived at as a projection of SetMap (e.g. Setmap t ()).

-- If keys are sets, then why not work with the original sets
-- directly, e.g. a partition: a set of disjoint sets.

-- You should probably use Data.Partition

-- https://en.wikipedia.org/wiki/Partition_of_a_set
-- https://en.wikipedia.org/wiki/Setoid
-- http://hackage.haskell.org/package/setoid-0.1.0.0/docs/Data-Setoid.html
-- http://hackage.haskell.org/package/data-partition-0.3.0.0/docs/Data-Partition.html


module Partition where
  
import Data.Set(Set)
import Prelude hiding (lookup)
import qualified Data.Set as Set

-- A partition is a set of disjoint sets.  When we refer to an element
-- of (Partition a), we mean a type (Set a).  Represented as [(Set a)]
-- instead of (Set (Set a)), because the outer set's operations would
-- need (Ord (Set a)), and we just need foldr and map on the outer
-- container.
newtype Partition a = Partition [(Set a)]

-- This is a "concrete" partition in that we do not use an abstract
-- equivalence relation.  Two sets are equivalent if they have
-- overlap.
overlap a a'  = not $ disjoint a a'
disjoint a a' = Set.null $ Set.intersection a a'

-- There seem to be two sorts of operations on partitions.

-- a) Those that leave the structure of the partition intact, allowing
-- to operate on the quotient set, e.g. using representatives, and

rep :: Ord e => Set e -> e
rep s = foldr max e0 es where (e0:es) = Set.toList s

reps :: Ord e => Partition e -> [(e, Set e)]
reps (Partition p) = map tag' p where tag' s = (rep s, s)

-- b) those that change the partitioning / equivalence relation.

-- Below, some routines are defined that generalize the quotient set
-- operations "insert", "remove", and "element" in such a way that if
-- the key sets used are proper partition elements they behave as
-- quotient set operations, but if they are not, they essentially
-- generate a new partition + implement the operation on the
-- transformed partition.

-- Essentially, introducing a new overlapping keyset into an existing
-- equivalence relation can result in some previously distinct
-- elements to become the same.

-- The generalizing is based on "coalesce", which restructures the
-- partition.  Think of drops of liquid on a surface coalescing when
-- an overlapping drop is added.

coalesce :: Ord e => Set e -> Partition e -> (Set e, Partition e)
coalesce keyset (Partition p) = foldr split (Set.empty, Partition []) p where

  split s (s', Partition p') =
    case overlap s keyset of
      
      -- If there is no overlap at all, the set is part of the new
      -- disjoint partition.
      False -> (s', Partition $ s:p')

      -- If there is overlap, the previously distinct element will be
      -- coalesced with the new partition element.
      True  -> (Set.union s s', Partition p')


-- The generalizations of the quotient set operations are defined in
-- terms of coalesce.

insert :: Ord e => Set e -> Partition e -> Partition e
insert k p = p' where
  (k', Partition disjoint) = coalesce k p
  p' = Partition $ (Set.union k k') : disjoint

remove :: Ord e => Set e -> Partition e -> Partition e
remove k m = snd $ coalesce k m

element :: Ord e => Set e -> Partition e -> Bool
element e m = not $ Set.null $ fst $ coalesce e m


    

