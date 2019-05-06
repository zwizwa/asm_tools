
-- Seq has no notion of time multiplexing: it is dataflow only,
-- mapping to the basic substrate of logic functions and register
-- elements.

-- To be able to represent time-multiplexed computations, we'll need
-- some notion of nested loops and storage.

-- The pattern that emerges in Algebra.hs is:
-- class (Ring m t, Traversable f, Zip f) => Vector m f t

-- It seems to be even a very limited subset of that:
-- . foldM
-- . traverse + zipWith

-- But why not try to implement it directly?


module Language.Seq.Loop where
import Langaguage.Seq

