module SMap where
import Data.Set(Set)
import Prelude hiding (lookup)
import qualified Data.Set as Set


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
-- it needs to be checked that this invariant holds.
newtype SMap a b = SMap { unSMap :: [(Set a, Set b)] }


empty :: SMap a b
empty = SMap []

insert :: (Ord a, Ord b) => Set a -> Set b -> SMap a b -> SMap a b
insert a b m@(SMap l) = case lookup' a m of
  Nothing ->
    SMap ((a,b):l)
  Just (a',b') ->
    SMap ((Set.union a a', Set.union b b') : (unSMap $ remove a' m))

remove :: Set a -> SMap a b -> SMap a b
remove = undefined

-- Non-constructors

lookup a m = fmap snd $ lookup' a m
         
lookup' :: Ord a => Set a -> SMap a b -> Maybe (Set a, Set b)
lookup' a (SMap l) = f l where
  f [] = Nothing
  f ((a',b):l) = case ntEqual a a' of
    True -> Just (a',b)
    False -> f l

-- Non-primitives

element :: (Ord a, Eq b) => Set a -> SMap a b -> Bool
element e m = Nothing /= lookup e m


-- Tools
-- non-transitive equal
ntEqual a a' = not $ Set.null $ Set.intersection a a'


