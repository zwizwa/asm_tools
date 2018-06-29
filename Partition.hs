
-- A projection of SetMap.  If keys are sets, then why not work with
-- the original sets directly, e.g. a partition: a set of disjoint
-- sets.

module Partition where
  
import Data.Set(Set)
import Prelude hiding (lookup)
import qualified Data.Set as Set

type Partition a = (Set (Set a))

overlap a a' = not $ Set.null $ Set.intersection a a'

partition :: Ord e => Set e -> Partition e -> (Set e, Partition e)
partition keyset p = foldr split (Set.empty, Set.empty) p where
  split s (s', p') =
    case overlap s keyset of
      False -> (s', Set.insert s p') -- add to disjoint partition
      True  -> (Set.union s s', p)   -- merge into overlapping singleton partition

insert :: Ord e => Set e -> Partition e -> Partition e
insert k p = p' where
  (k', disjoint) = partition k p
  p' = Set.insert (Set.union k k') disjoint




-- Non-primitives

remove :: Ord e => Set e -> Partition e -> Partition e
remove k m = snd $ partition k m

element :: Ord e => Set e -> Partition e -> Bool
element e m = not $ Set.null k where
  (k, _) = partition e m



