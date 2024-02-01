-- Picking up 2024-02-01
--
-- . Last summer I started playing with DSP langauge
--
-- . Might be good to do something simpler first: RTL, and find a way
--   to link library code to existing Seq implementation.

-- Older notes below.

-----------------------------------------------------------------------

-- Experimenting with Dependent Types.

-- Main idea is to represent signals in such a way that more
-- information is encoded at compile time.  E.g sampling rate, context
-- (piece-wize FFT) etc..


-- Exploring type level programming / dependent types in Haskell.

-- Asked on #haskell IRC channel about state of dependent types.  Answer was:
-- To emulate:
-- https://hackage.haskell.org/package/singletons-3.0.1
-- Where things are going:
-- https://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf

-- Eisenberg's thesis seems like a good summary of what extensions are
-- useful already.

-- Where I want to go with this is to express all iteration patterns I
-- have run into while writing DSP code, more specifically:
--
-- . Converting between linear and blocked views (time/frequency)
-- . Using "temporal" algorithms in a "spatial" way
-- . Representing triangular O(N^2) algorithms

-- Compared to older approachs, I definitely want to get rid of Seq's
-- "dynamic" dependence on signal sizes, so maybe start there?  That
-- said the music DSP angle is more important atm than the FPGA angle,
-- so let's stick with simpler substrates.


-- As for "blocked" view of a signal, the focus is on the two-way
-- function (not always bijection)
--
-- S T <-> S (B T)
--
-- which converts between signal of primitive type T and signal of
-- block of primitive type T.
--
-- I want lift all Functor and Applicative operations that work on S T
-- and have them available on S (B T).


module Language.SeqDT where

data AB = A | B

test = ()





