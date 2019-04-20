{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Seq.Algebra where

-- FIXME: There is a vague, non trivial collection type classes in my
-- head that would make it a lot easier to express DSP algorithms at a
-- higher level.  It will require quite a bit of leverage through
-- nesting, so structure and mostly absence of arbitrary limitations
-- is VERY important.

-- At this point, there are a couple of constraints:
--

-- . Do not mix mathematical structure with representation (the 'r'
--   parameter).  Representation is really a metaprogramming concept and
--   as such lower level than e.g. a Ring structure.
--
-- . Separate mathematics (e.g. Ring) from implementation (Seq)
--
-- . Keep Ring very abstract.  This likely means monadic zero and one,
--   which is annoying but pays off in abstraction power.
--
-- . Keep linear functions abstract.  I.e. matrices are a special case.
--
-- . Separate the ideas of Z transform (Difference equations in a
--   Ring), and differentiable programs, which are necessary to
--   express non-linearity.
--
-- . Polynomials should have a special place as well, mainly as
--   approximation of non-linear functions, but also as Ring elements.
--
-- . Implementation should be recursive.  I.e. flatten Seq to Seq
--   without MUL, or flatten transcendental functions into
--   polynomials or more elaborate "control rate" structures.

-- All these should compose well, so don't add unnecessary obstacles.

-- The big idea here seems to be parameterized Z-transforms: i.e. an
-- update function that is linear in a couple of parameters, but as
-- coefficients that might be transcendental.



-- For DSP algorithm specification it is useful to have a sub-language
-- that focuses on algebraic laws.  It should support:

-- . linear functions
-- . polynomials
-- . matrices
-- . z-transforms  (linear functions + feedback)
-- . automatic differentiation
  


  
-- The basic substrate for all computations is the the Ring.  These
-- can be composed into matrices, which we will represent abstractly
-- as linear functions.

-- Do these need to be wrapped in representation constructors?  Only
-- if functions themselves need to be represented.  Maybe do that
-- separately, as it is already complex enough.


class Monad m => Ring m t where
  add   :: t -> t -> m t
  sub   :: t -> t -> m t
  neg   :: t -> m t
  mul   :: t -> t -> m t

  -- Note that it would be nice to have nesting, but this likely
  -- requires representation of functions as well.  It might be best
  -- to put that in a separate class.
   
  -- apply :: (t -> m t) -> t -> m t
  --   vs
  -- apply :: (r (t -> m t)) -> (t -> m t)

  -- Implementation note: a (t -> m) functional dependency would allow
  -- to keep constants non-monadic.  Monadic constants are a real pain
  -- to work with, but somehow are more honest.  I.e. it might be that
  -- there is no such a thing as a "cheap" constant in a particularly
  -- involved instance, so always let the Monad decide how to produce
  -- it.  At the very least, this allows sharing.  It is absolutly
  -- necessary to have multipe t be associated to a single m
  -- (e.g. base type + matrices).

  zero  :: m t
  one   :: m t

-- Use a short name for this that is easy to work with, since it will
-- occur frequently in construction and matching.
data C t = C t t

instance Ring m t => Ring m (C t) where
  add = c2 add
  sub = c2 sub
  neg = c1 neg
  mul (C ar ai) (C br bi) = do
    ar_br <- mul ar br ; ai_bi <- mul ai bi ; a <- sub ar_br ai_bi
    ar_bi <- mul ar bi ; ai_br <- mul ai br ; b <- add ar_bi ai_br
    return $ C a b
  zero = do zero' <- zero ;               return $ C zero' zero'
  one  = do zero' <- zero ; one' <- one ; return $ C one'  zero'

c1 f (C ar ai)           = do cr <- f ar    ; ci <- f ai    ; return $ C cr ci
c2 f (C ar ai) (C br bi) = do cr <- f ar br ; ci <- f ai bi ; return $ C cr ci
    

-- Note that the Ring maps directly to Seq with multiplication, so a
-- program that contains nested Ring instances should be able to
-- flatten to a base Ring and be implemented directly in SeqMul.

-- TODO: This is a test case.  Create a complex filter by nesting 3 rings:
-- . abstract 1-pole
-- . complex numbers
-- . transfer functions (analysis only)


-- Systems defined by difference equations.  This is implemented as
-- tucking away state into the monad, leaving only input and output
-- exposed.  It is currently not very clear on how to do this right.
-- FIXME: It is not clear how exactly to mesh this with two
-- conflicting constraints:

-- Linear systems are different, because they can be z-transformed.
-- Non-linear systems are implementable, but only analyzable when
-- linearized at a point.


-- Note that initial values are monadic as well.  They might need to
-- be computed.

class Ring m t => System m t where
  -- system init update = closed.
  system :: m t -> (t -> t -> m (t, t)) -> (t -> m t)


-- Does a system need to be a class?  Can it just be a data type?  In
-- Seq I've found it very useful to be able to mix systems and I/O
-- relations to build composite systems.  The state representation is
-- then hidden from view.  I want to do this, without loosing the
-- ability to perform analysis on the system.



-- FIXME: Clean up comments below.




-- There are two "directions" of extension for the Ring:
--
-- . parameterized ring: allow for transentental functions to compute
--   coefficients.
--
-- . feedback and z-transform

-- On top of that are mostly implementation details such as loop structure.



-- It is important to keep Ring structure and differentiable structure
-- separate.  Once way to think about it is that transcendental
-- functions are always somehow defined at the meta level.  They are
-- useful in code (specification) but will be ephemeral in
-- implementation (i.e. used in autodiff at compile time to derive
-- linearized update equations, and replaced by polynomals or
-- iterative systems in implementations).

-- This is important because in order to define a Z transform, the
-- basic substrate still needs to be a Ring.  A nonlinear equation can
-- be linearized into a Ring, and the nonlinear parameterization can
-- be used in automatic differentiation.

-- Sorry I find this hard to express.  Take a second pass later.

-- Instead of non-linear, focus on non-polyomial.  

class Ring m t => NonPoly m t where
  div  :: t -> t -> m t
  sin  :: t -> m t
  cos  :: t -> m t
  exp  :: t -> m t
  sqrt :: t -> m t
  


-- Second pass: I want to be able to create systems that can be
-- differentiated.  And the output of the differential should be a
-- linear function that has a parameterized Z transform.

-- system -> differential -> z-transform






  
-- Given a function in that Ring, it can be implemented as a seq.
  


-- Seq mostly aims at implementation substrate, i.e. basic
-- implementable digital logic + some multiplier platform (either
-- recursively defined as a circuit, or targeting FPGA multiplier
-- blocks).

  



-- Library




-- Tests
