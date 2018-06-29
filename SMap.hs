module SMap where
import Data.Set(Set)
import Prelude hiding (lookup)
import qualified Data.Set as Set

-- FIXME: This is inconsistent.  The problem is that a lookup can
-- match multiple things.  How to deal with that?


-- Interesting problem though, but there has to be a simpler way.

-- In the context of netlists...

-- To implement shorting properly, it makes sense to introduce the
-- idea of aliases, e.g. (Set NetName).  But to make this work in the
-- standard way it seems Eq and Ord are needed.  Eq based on
-- non-disjunction already violates transitivity.  See
-- papers/haskell.txt

-- It does seem possible to define a weaker notion of equality
-- locally, inside the implementation of a Map-like abstraction.

-- Anyways, the details go deeper than expected.  So stick to the
-- basics.  We need a set-indexed map, mapping sets to sets.

-- TODO: Either this is meaningless, or there is already an
-- abstraction like this.


-- Invariant: all elements of the list are unequal in terms of the
-- transitive closure of non-disjointness.  For each constructor below
-- it needs to be checked that this invariant (I) holds.
newtype SMap a b = SMap { unSMap :: [(Set a, Set b)] }


-- I: trivial
empty :: SMap a b
empty = SMap []

-- I: reduction, so I->I
remove :: Ord a => Set a -> SMap a b -> SMap a b
remove a (SMap l) = SMap $ filter (not . equal a . fst) l

-- I: doesn't hold.
insert :: (Ord a, Ord b) => Set a -> Set b -> SMap a b -> SMap a b
insert a b m@(SMap l) = m' where
  (a', b') = lookup' a m
  m' = case Set.null a' of
    True -> 
      SMap ((a,b):l)
    False ->
      SMap ((Set.union a a', Set.union b b')
            : (unSMap $ remove a' m))


-- Non-constructors

-- lookup a m = fmap snd $ lookup' a m


-- Beware of the analogue of "lookup".  (FIXME: rename it as "collect" ?)
--
-- Given the map
--   {1,2} => {a}
--   (3,4} => {b}
--
-- What is lookup {1,3} ?  Based on the local definition of equalitly,
-- to be {a,b}, so it is clear that lookup performs some collapse
-- based on the key.
--
-- In that context it makes sense to return both the new name and value.
-- It's probably a good idea to also implement partitioning along the way.

lookup' :: (Ord a, Ord b) => Set a -> SMap a b -> (Set a, Set b)
lookup' a (SMap l) = kv where
  kv = foldr merge (Set.empty, Set.empty) l
  merge (a, b) s@(a', b') = case equal a a' of
    True  -> (Set.union a a', Set.union b b')
    False -> s





-- Non-primitives

element :: (Ord a, Ord b) => Set a -> SMap a b -> Bool
element e m = not $ Set.null ks where
  (ks,_) = lookup' e m


-- Tools
-- non-transitive equal
equal a a' = not $ Set.null $ Set.intersection a a'


