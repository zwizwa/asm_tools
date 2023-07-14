{-# LANGUAGE
AllowAmbiguousTypes,
MonoLocalBinds,
ScopedTypeVariables
#-}

-- A note on the extensions.
--
--
-- ScopedTypeVariables because it just seems to be a better default.
-- Also seems to be necessary in some cases, mostly to constrain the
-- monad.
--
-- MonoLocalBinds, AllowAmbigious types: added from a ghc suggestion.
-- Can't say I really understand.
--
-- 


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
  add  :: a -> a -> m a
  mul  :: a -> a -> m a

-- Note that this monad only contains things like local variable
-- context to implement sharing, and cannot contain things like
-- feedback register state.  That distinction is important (because
-- the same "language monad m" will be active for a variety of state
-- types).

-- Define the concrete signal form as a monadic version of s->(s,a)
-- with initial s and the implementation monad m that abstracts the
-- target evaluation sequencing.
data Sig m s a = Sig s (s -> m (s, a))


-- Let's get into the habit of writing down the types explicitly.
-- Inference doesn't always work or we want to constrain more than
-- what inference produces. And anyway, the types are really the point
-- of the exercise.

-- A lot of operations can be defined on signals, without knowing what
-- kind of signal it is (see Audio and Control below).

-- Create a constant signal.
constSig :: PrimOp m a => a -> Sig m () a
constSig c = Sig () (\() -> return ((), c))

-- FIXME: more signal ops


-- At the DSL level we can have different signal types, e.g. one for
-- control rate and one for audio rate.

-- This is then abstracted in the user signal types, one for control
-- rate and one for audio rate.  The implementation monad m is kept as
-- a parameter to distinguish different implementations.
data Control m a = forall s. SigImpl m s a => Control (Sig m s a)
data Audio   m a = forall s. SigImpl m s a => Audio   (Sig m s a)

-- The implementation of the semantics is hidden behind this class.
class SigImpl m s a where
  compileSig :: (Sig m s a) -> String

-- The definition of signal combinators does not need to be part of
-- SigImpl: the code is the same for all implementations (i.e. the
-- behavior of the combinators is the same).
scaleSig (Sig s0 u) a = Sig s0 u' where
  u' s = do
    (s', o) <- u s
    a' <- a `mul` o
    return $ (s', a')

scaleAudio :: PrimOp m a => Audio m a -> a -> Audio m a
scaleAudio (Audio sig) a = Audio $ scaleSig sig a

scaleControl :: PrimOp m a => Control m a -> a -> Control m a
scaleControl (Control sig) a = Control $ scaleSig sig a

-- To simplify, use Float as the base type so focus can be on the
-- combinators.  Generalization to other base types can be solved
-- later.
--

-- A simple processor that is built friom a combinator: scale the
-- amplitude of an audio signal by 50%.
ex_half :: PrimOp m Float => Audio m Float -> Audio m Float
ex_half audio = scaleAudio audio 0.5

-- Constant signal
ex_ones :: (SigImpl m () a, PrimOp m a, Num a) => Audio m a
ex_ones = Audio $ constSig 1

-- Apply processor to signal
ex_halves :: (PrimOp m Float, SigImpl m () Float) => Audio m Float
ex_halves = ex_half ex_ones

-- Counter
ex_ramp :: (PrimOp m Float, SigImpl m Float Float) => Audio m Float
ex_ramp = Audio $ Sig 0 (\s -> do s' <- s `add` 1; return (s', s))


-- Now, how to convert this into a concrete program?  E.g. a Haskell
-- program that operations on lists.  In this case the implementation
-- monad can just be Identity.  It is wrapped because we want to use
-- this as a type tag, where the concrete Identity is not appropriate.

newtype HsList a = HsList (Identity a) deriving (Functor, Applicative, Monad)




-- First set up some concrete implementation infrastructure in terms
-- of Sig, i.e. where state is explicit.  This is just an example,
-- i.e. too specialzed to be usefulf for generic code.  Here hf_
-- refers to Haskell Float, where the monad can just be HsList.

instance PrimOp HsList Float where
  mul a b = return $ a * b
  add a b = return $ a + b

-- Example signal
hf_1 = constSig 1
hf_1 :: Sig HsList () Float

-- Compiles signal representation to concrete infinite list.
hf_list :: Sig HsList () Float -> [Float]
hf_list (Sig s0 u) = o:os where
  HsList (Identity (s', o)) = u s0
  os = hf_list (Sig s' u)

hf_listf :: Sig HsList Float Float -> [Float]
hf_listf (Sig s0 u) = o:os where
  HsList (Identity (s', o)) = u s0
  os = hf_listf (Sig s' u)



hf_1trunc = take 5 $ hf_list hf_1



-- The final step is to implement compilation from the generic version
-- of a signal to whatever the target representation is.  Note that it
-- is enough to focus on compiling signals.  Compiling processors can
-- be done implicitly by applying a representation of the input
-- parameters.

-- Start with the simples thing: a constant Float signal
a1 :: forall m. (PrimOp m Float, SigImpl m () Float) => Audio m Float
a1 = Audio $ constSig 1

-- Create an implementation instance. 
instance SigImpl HsList () Float where
  compileSig sig = show lst where
    lst = take 5 $ hf_list sig

instance SigImpl HsList Float Float where
  compileSig sig = show lst where
    lst = take 5 $ hf_listf sig

-- Specialized compiler function.
hf_comp :: SigImpl HsList () a => Audio HsList a -> String
hf_comp (Audio sig) = compileSig sig

hf_compf :: SigImpl HsList Float a => Audio HsList a -> String
hf_compf (Audio sig) = compileSig sig


test_hf0 = hf_comp a1
test_hf1 = hf_comp $ ex_half a1

-- Not sure why I had to specify so many things, but it works.  Maybe
-- continue building more complex examples.

-- Next is a counter.  This can be added to the combinators.  First
-- just write out.

test_hf2 = hf_compf ex_ramp











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


