-- Finite functions from sets to sets.

-- TODO: This seems to make sense, so there is likely an existing
-- implementation.

module SMap where
import Data.Set(Set)
import Prelude hiding (lookup)
import qualified Data.Set as Set

-- In the context of netlists...
-- See papers/haskell.txt for more information

-- To implement shorting properly, it makes sense to introduce the
-- idea of aliases, e.g. instead of indexing nets based on a key
-- NetName, index them as (Set NetName).

-- This suggests the avenue of Map (Set a) b with two keys being
-- inqual if the sets are disjoint.  However, that operation is not
-- transitive.  Map needs Eq and Ord, so it doesn't seem like that
-- will work out.

-- It does seem possible to define a weaker notion of equality
-- locally, inside the implementation of a Map-like abstraction.

-- Following through with an attempt to implement this leads to an
-- invariant (keys in the map should be disjoint), and a central
-- nontrivial "partition" operation.



-- Invariant: all elements of the list are unequal in the sense that
-- their key sets are disjoint.  For each constructor below it needs
-- to be checked that this invariant (I) holds.
newtype SMap a b = SMap { unSMap :: [(Set a, Set b)] }


-- The non-transitive generalized equality operator.

-- ( TODO: is there a way to represent this so it can be transitively
-- closed?  One idea is to turn it around, and actually use this
-- implementation mapping to () as a representation. )

overlap a a' = not $ Set.null $ Set.intersection a a'



-- I: trivial
empty :: SMap k v
empty = SMap []

-- I: reduction, so I->I
remove :: Ord k => Set k -> SMap k v -> SMap k v
remove k (SMap kvs) = SMap $ filter (not . overlap k . fst) kvs

-- I: induction: if I holds for the input sets, it holds because we
-- are joining a matching singleton with a disjoint partition.
insert :: (Ord k, Ord v) => Set k -> Set v -> SMap k v -> SMap k v
insert k v m@(SMap l) = m' where
  ((k', v'), SMap disjoint) = partition k m
  m' = SMap $ case Set.null k' of
    True  -> ((k,v):l)
    False -> ((Set.union k k', Set.union v v') : disjoint)



-- The key routine is "partition", which is a generalization of
-- "lookup", returning not only the requested value, but also the
-- maximum of the key, and the maximal disjoint submap.
--
-- Motivation: given the map
--   {1,2} => {a}
--   (3,4} => {b}
--
-- What is lookup {1,3} ?  Based on the local definition of equalitly,
-- there seems to be no other sensible answer than {a,b}.  However in
-- that context, the key we used to perform the lookup can be
-- maximized to {1,2,3,4}.
--
-- So it makes sense to return both the value and the maximized key.  
--
-- In the context of other operations, it seems also natural to
-- collect and return the disjoint part.  This is useful for
-- implementing insert.


-- I: induction: only partitions are performed to construct the output
-- map, so I holds if it it holds for the input.

partition :: (Ord a, Ord b) => Set a -> SMap a b -> ((Set a, Set b), SMap a b)
partition a (SMap l) = p where
  p = foldr merge ((Set.empty, Set.empty), SMap []) l
  merge p@(a, b) (c@(a', b'), ps'@(SMap ps)) = case Set.null a of
    True  -> (c, SMap (p:ps))
    False -> ((Set.union a a', Set.union b b'), ps')


-- Other Non-primitives
element :: (Ord a, Ord b) => Set a -> SMap a b -> Bool
element e m = not $ Set.null ks where
  ((ks, _), _) = partition e m



