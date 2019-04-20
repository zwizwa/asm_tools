{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- I've split this up a bit:

-- Language.Seq.Algebra contains types and classes to implement
-- algorithms and functions at the highest possible level.  It
-- leverages composition made possible by keeping classes very
-- abstract.

-- This module is a bridge between those "math oriented" routines, and
-- the Seq implementation language.  It contains a partial
-- instantiation.

-- Note that this is a generic pattern: It is important to distinguish
-- lambda calculus from algebraic structure (in first iteration.
-- Later a more direct bridge will become apparent).


-- Code that assumes multiplers are implemented.  Note there is
-- currently no "meet" between Seq for Verilog and multipliers.  I'm
-- approaching the problem from a couple of sides
--
-- . The C target supports MUL.  This will be used in practice to
--   prototype some DSP algorithms.
--
-- . The iCE ECP5 has multipliers and can serve as a realistic
--   target: TinyFPGA EX (when it's done).
--
-- . If still needed, mul can be implemented in terms of low level
--   "nested Seq".  However this case is not urgent as it will likely
--   not be used in practice as FPGAs with multipliers are available.

--
-- The future goal is to use Seq instance granularity to specify an
-- algorithm in dataflow, and create "guided instantiation macros" to
-- flatten it down to be time-multiplexed.  I have only a very vague
-- idea about how to tacke it, so I'm writing some applications first
-- to maybe bring some concrete path within reach.

-- Note that "unit delay" for a DSP program implemented on a CPU is
-- not going to be the clock cycle of the processor, but the round
-- trip of the sample processing loop.  The same concept is valid
-- though.

module Language.Seq.DSP where

import Language.Seq         as Seq
import Language.Seq.Lib     as SeqLib
import Language.Seq.Algebra as A

import Language.Seq.Term    as Term
import Language.Seq.C       as C

-- This is not a true Ring in the mathematical sence, as finite word
-- lengths are used in the implementation.
instance Seq.Seq m r => A.Ring m (r S) where
  add  = Seq.add
  sub  = Seq.sub
  neg  = Seq.neg
  mul  = Seq.mul
  zero = return 0
  one  = return 1



-- There is some ambiguitiy that I don't know how to resolve
-- generically.  
--
--      Matching instances:
--        instance Seq m r => Ring m (r S)
--        instance [safe] Ring m t => Ring m (C t)
--
-- It is somewhat obvious to me that the last one is the one you want,
-- and the first one is some arbitrary clash.  It might not be
-- necessary to resolve this generaically, as instantiatiating to a
-- specific Seq instances fixes it.  Anyway it might be as simple as
-- constraining r using specific instances.  It is obvious that C is
-- not a representation r, but the compiler doesn't know that.


poing :: Ring m t => t -> t -> m (t, t)
poing phasor state = do
  next <- A.mul phasor state
  return (next, next)


-- FIXME: In practice, the Term.compileTerm context will fix the type
-- so the ambiguity is not an issue and explicit annotation is not
-- necessary.

-- Instantiate to specific Seq instance used for compilation to C.
-- poing' :: Term.R S -> Term.R S -> Term.M (Term.R S, Term.R S)
-- poing' = poing


-- An example: a complex one pole.



-- DSP algorithms will be implemented in their "natural habitat",
-- i.e. an algebraic Ring structure.






-- To get started, I want to implement a simple spectral line filter.
-- This already contains a lot of concept that might not be
-- expressible.  It is a block-processing algorithm
--
-- . Compute the inner product between the signal vector and a complex
--   sinusoid.  The inner product is the output (block -> phasor)
--
-- . Update the phasor generator for the next step + normalize
--
-- The point is to make the combinators and construct a "recursive"
-- Seq.  The point of Seq is modularity and composition.
--
-- Before all that, focus on the combinator structure using just an
-- exponential.





-- damp a = do
--   t <- stype a
--   reg1 t $ \s -> mul a s




run = do
  putStrLn "Language.Seq.DSP.run"
  let m = do
        let typ = undefined
        closeReg [typ] $
          (\[s] -> do
              -- FIXME: I would need either fixed point mul or
              -- floating point types.
              one' <- A.one
              (s',o) <- poing one' s
              return ([s'],[o]))
      (outputs, bindings, probes) = Term.compileTerm m
  putStrLn $ C.compile "testfun" (outputs, bindings, probes)
  
