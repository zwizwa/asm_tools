{-# LANGUAGE
MonoLocalBinds
#-}

module Language.DSP.Test where

import Control.Monad.Identity

-- See also older drafts at the bottom.  Trying to figure out the
-- simplest version of the problem.

-- A simpler mapping:
--
-- . MAIN IDEA: The user of the DSL "instantiates combinators".  The
--   combinators are defined as manipulation of C syntax.  This should
--   avoid monads in the DSL itself.
--
-- . IMPLEMENTATION: The base language (the substrate) is a dataflow
--   language that lives inside a Monad which implements e.g. local
--   variable sharing.
--

-- Note that a lot of magic is encoded in the implementation of the
-- combinators on top of the base language.  It seems best to provide
-- a Haskell implementation of the base language as well.

-- An example of what needs to be expressible at the DSL level is the
-- application of a pure function (a,b)->c to the signal rate signal S
-- a and the control rate signal C b and yield a signal rate signal S
-- c.
--
-- :: (a -> b -> c) -> (S a -> C b -> S c)
--
-- Here 'S' and 'C' are fairly abstract.  The way these containers are
-- implemented can be quite involved.


-- Let's first try to write down the types and some example
-- operations.  At the lowest level there is the scalar language with
-- operations in a monad.
class Monad m => PrimOp m a where
  add :: a -> a -> m a
  mul :: a -> a -> m a

-- Note that this monad only contains things like local variable
-- context to implement sharing, and cannot contain things like
-- feedback register state.  That distinction is important (because
-- the same "language monad m" will be active for a variety of state
-- types).

-- Define the concrete signal form as a monadic version of
-- s->(s,a) with initial s and the implementation monad m.
data SSO m s a = SSO s (s -> m (s, a))


constSSO c = SSO () (\() -> return ((), c))

-- This is then abstracted in the user signal types, one for control
-- rate and one for audio rate.
data C a = forall s m. SSOImpl m s a => C (SSO m s a)
data A a = forall s m. SSOImpl m s a => A (SSO m s a)

-- The base language implementation can then be hidden behind a type
-- class.
class SSOImpl m s a where
  scaleSSO :: (SSO m s a) -> a -> (SSO m s a)

instance PrimOp m a => SSOImpl m s a where
  scaleSSO (SSO s0 u) a = SSO s0 u' where
    u' s = do
      (s', o) <- u s
      a' <- a `mul` o
      return $ (s', a')
  

scaleA :: A a -> a -> A a
scaleA (A sso) a = A $ scaleSSO sso a

scaleC :: C a -> a -> C a
scaleC (C sso) a = C $ scaleSSO sso a

prog_half audio = scaleA audio (0.1 :: Float)

-- Now, how to convert this into a concrete program?  E.g. a Haskell
-- program that operations on arrays.  In this case the implementation
-- monad can just be Identity.

instance PrimOp Identity Float where
  mul a b = return $ a * b
  add a b = return $ a + b

-- Example signal
h1 = constSSO 1
h1 :: SSO Identity () Float
h1A = A h1

-- Compiles signal representation to concrete infinite list.
hlist :: SSO Identity () Float -> [Float]
hlist (SSO s0 u) = o:os where
  Identity (s', o) = u s0
  os = hlist (SSO s' u)

-- hlistA (A sso) = hlist (sso :: SSO Identity () Float)
  
test = take 5 $ hlist h1

--hlist (A (SSO s0 u)) = o : os where
--  Identity (o, mos) = do
--    (s', o) <- u s0
--    return (o, 
    
  
  









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



-- Draft 2
--
-- Not so important (It's more important to first do a Haskell
-- implementation on top of a monadic language, then later figure out
-- C side implementation details)
--
-- . For now assume that 1) every _combinator result_ has a C language
--   implementation (e.g. no blocking state machine code), and 2) C
--   structs can be used to bundle variables, assuming the compiler
--   will optimize away the intermediate structs.  If those
--   assumptions fail it would still be ok to revisit later.  Note
--   that the combinator itself might not be representable in C
--   syntax, only as a map from C syntax to C syntax.
--
-- . Use integer-indexed types at the Haskell level.  Full dependent
--   types are probably not necessary, but I do need to be able to
--   specialize code to a fixed vector size.
--
-- . C syntax can be replaced by an embedding of a subset of C/LLVM
--   inside Haskell.  It seems simpler to implement the heavy lifting
--   (the syntax combination) inside Haskell.  (EDIT: Can't actually
--   implement the combinators in C).
--


