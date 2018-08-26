-- Finite functions from sets to sets.

-- Note: this is an intermediate step in realizing that the key
-- representation is the partition (Set (Set t)), and that the key
-- operation is the "partition" operation below, but mapped to ().

-- Basically, once partitions are recognized as their own thing, the
-- naming is no longer needed and can even be embedded inside of the
-- set.

-- The apparent need for this approach is insistence on names, which
-- are not appropriate.  A better central idea is "representative".


module Data.AsmTools.SetMap(SetMap,empty,overlap,partition,insert,remove,element,toList) where

import Data.Set(Set)
import Prelude hiding (lookup)
import qualified Data.Set as Set

-- Discovered in the context of creating shorts in a circuit netlist.
-- See papers/haskell.txt for more information

-- To implement shorting properly, it makes sense to introduce the
-- idea of aliases, e.g. instead of indexing nets based on a key
-- NetName, index them as (Set NetName), where the set is the
-- collection of original names of the now shorted nets.

-- This suggests the avenue of Map (Set a) b with two keys being
-- inqual if the sets are disjoint.  However, that operation is not
-- transitive.  Map.insert needs Eq and Ord, so it doesn't seem like
-- that will work out.

-- Below has a variation of the standard map operations (insert,
-- lookup, remove) which is based on the invariant that keys in the
-- map should always be disjoint.  Attempting to construct "lookup"
-- fails, but correcting that attempt leads to a central nontrivial
-- "partition" function.

-- For each constructor below it needs to be checked that this
-- invariant (I) holds.
newtype SetMap a b = SetMap { toList :: [(Set a, Set b)] }


-- The non-transitive generalized equality operator.

-- ( TODO: is there a way to represent this so it can be transitively
-- closed?  One idea is to turn it around, and actually use this
-- implementation mapping to () as a representation. )

overlap a a' = not $ Set.null $ Set.intersection a a'



-- I: trivial base case
empty :: SetMap k v
empty = SetMap []

-- The main operation is "partition", which is a generalization of
-- "lookup", returning not only the requested value, but also the
-- maximum of the key wrt. matching keys, and the maximal disjoint
-- submap.
--
-- Motivation: given the mapping
--   {1,2} => {a}
--   (3,4} => {b}
--   {5,6} => {c}
--
-- What is lookup {1,3} ?  Based on the local definition of equalitly,
-- there seems to be no other sensible answer than {a,b}.  However in
-- that context, the key we used to perform the lookup can be
-- maximized to {1,2,3,4}.
--
-- So it makes sense to return not only the value but also the
-- maximized key.
--
-- In the context of other operations, it is natural to collect and
-- return the disjoint part.  This is useful for implementing insert.

-- The operation is called "partition" because it creates a map
-- partition of a singleton map (represented explicitly as a pair) and
-- a remainder map.

-- Note: below, the assymetry between key and value sets is in that
-- overlap is only called for the keys.

-- I: induction: only partitions are performed to construct the output
-- map, so I holds if it it holds for the input.

partition :: (Ord k, Ord v) => Set k -> SetMap k v -> ((Set k, Set v), SetMap k v)
partition k (SetMap kvs) = p where
  p = foldr merge ((Set.empty, Set.empty), SetMap []) kvs
  merge kv@(k, v) (kv'@(k', v'), kvs'@(SetMap kvs)) =
    case overlap k k' of
      False -> (kv', SetMap (kv:kvs))
      True  -> ((Set.union k k', Set.union v v'), kvs')


-- I: induction: if I holds for the input sets, it holds because we
-- are joining two disjoint key sets.
insert :: (Ord k, Ord v) => Set k -> Set v -> SetMap k v -> SetMap k v
insert k v m@(SetMap l) = m' where
  ((k', v'), SetMap disjoint) = partition k m
  m' = SetMap ((Set.union k k', Set.union v v') : disjoint)




-- Non-primitives

remove :: (Ord k, Ord v) => Set k -> SetMap k v -> SetMap k v
remove k m = snd $ partition k m

element :: (Ord a, Ord b) => Set a -> SetMap a b -> Bool
element e m = not $ Set.null ks where
  ((ks, _), _) = partition e m



