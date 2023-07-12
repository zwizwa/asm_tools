{-# LANGUAGE
MonoLocalBinds
#-}

module Language.DSP.Test where

-- See also older drafts at the bottom.  Trying to figure out the
-- simplest version of the problem.

-- A simpler mapping:
--
-- . The user of the DSL "instantiates combinators".  The combinators
--   are defined as manipulation of C syntax.  This should avoid
--   monads in the DSL itself.
--
-- . For now assume that 1) every _combinator result_ has a C language
--   implementation (e.g. no blocking state machine code), and 2) C
--   structs can be used to bundle variables, assuming the compiler
--   will optimize away the intermediate structs.  If those
--   assumptions fail it would still be ok to revisit later.  Note
--   that the combinator itself might not be representable in C
--   syntax, only as a map from C syntax to C syntax.
--
-- . C syntax can be replaced by an embedding of a subset of C/LLVM
--   inside Haskell.  It seems simpler to implement the heavy lifting
--   (the syntax combination) inside Haskell.
--
-- . Use integer-indexed types at the Haskell level.  Full dependent
--   types are probably not necessary, but I do need to be able to
--   specialize code to a fixed vector size.


-- Some key points
-- . The DSL at the Haskell end is combinator only
-- . Primitive combinators in C (or C wrapper in Haskell)
-- . No monads in the DSL: it maps signals to signals


-- Note that a lot of magic is encoded in the combinators, e.g. think
-- of the causal feedback state introduction which is far from
-- trivial.

-- Example: apply the pure function (a,b)->c to the signal rate signal
-- S a and the control rate signal C b and yield a signal rate signal
-- S c.
--
-- :: (a -> b -> c) -> (S a -> C b -> S c)
--
-- Here 'S' and 'C' are fairly abstract.  The way these containers are
-- implemented can be quite involved.


-- Next: write down the full type, then try to create an example (a
-- proof).  Because the types are so abstract, all the detail is going
-- to be in the implementation of the combinator _and_ the
-- representation of things like S a, C b.
--
-- From previous experiments: the signals are a pair of initial value
-- and generator function.  Thinking about making all initial values
-- zero, but for some finite iterative processes that might not be a
-- good idea.
--
-- Both C a and S a have the same shape: initial value and generator:
-- (s,s->(s,o)) but they are different in that they have a different
-- sample rate, which means their different semantics is only
-- observable in the implementation of a combinator.
--
-- Additionally, the primitive language's operators are in a Monad.
-- This is necessary to implement context, like sharing of
-- intermediate variables.


data SSO s m a = SSO s (s -> m (s, a))
-- constSSO c = SSO $ return c

class Monad m => PrimOp m a where
  add :: a -> a -> m a
  mul :: a -> a -> m a

-- The base language implementation can then be hidden behind a type
-- class.
class SSOImpl s m a where
  scaleSSO :: (SSO s m a) -> a -> (SSO s m a)

instance PrimOp m a => SSOImpl s m a where
  scaleSSO (SSO s0 u) a = SSO s0 u' where
    u' s = do
      (s', o) <- u s
      a' <- a `mul` o
      return $ (s', a')
  
-- data S a = forall s. SSOImpl s m a => S (SSO s a)
--data C a = forall s. SSOImpl s a => C (SSO s a)

--scaleS :: S a -> a -> S a
--scaleS (S sso) a = S $ scaleSSO sso a

--scaleC :: C a -> a -> C a
--scaleC (C sso) a = C $ scaleSSO sso a





test = do
  putStrLn "Language.DSP.Test"








-- Draft 1

-- Compile HOAS to C.  This is modeled after Lure seq.lua except that
-- all DSL type business is handled in the Haskell type system.

-- Compilation works by "passing a semantics to HOAS".  In Haskell
-- this is implemented as a type class parameterized by a
-- representation monad.

-- First: what is the compilation target?  Nested loops.  The purpose
-- of the DSL is mostly in combinators.  The basic operators are all
-- <lvalue> = <op> <rvalue> ...

-- I know I spent a lot of time trying to find a good representation
-- of "continuation".  A representation of a continuation could be:
-- the (indexed) variable where the result is stored + next
-- instruction to execute.  The control flow (next instruction) can be
-- implicit for now.  However it seems important to make the base
-- representation into CPS such that control structures can be added
-- later.

-- So the basic form is "dataflow as reduced CPS" or something.

-- A toy example would be adding two vectors, which is an operation
-- that takes a scalar CPS function and transforms it into a vector
-- CPS function.

-- To simplify, I am going to assume that the "code output" can be
-- written linearly.

-- Focusing on combinators, I need things like: convert async event
-- stream + audio rate stream into audio rate stream, interpolating
-- async events.

-- Previous attempts have always put the CPS into the monad.

-- Maybe this is all too lowlevel.  Focus on the combinators, the
-- algebra.  What the DSL compiler needs to do is to decide to inline
-- a combinator, or to implement it using a run-time indirection.
-- That is all really.

