{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Seq.Algebra where

import Control.Monad

import Data.Key(Zip(..),zipWith, zip)
import Prelude hiding (zipWith, zip, exp, cos, sin, sqrt)



-- Origin notes

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

-- Note that this does not contain any representation wrappers.  That
-- is a property of the implementation languages (e.g. Seq), not the
-- algebraic side.

-- Constants zero and one are implemented monadically.  A functional
-- dependency (t -> m) would allow to keep constants non-monadic.
-- Monadic constants are a real pain to work with, but somehow are
-- more honest.  I.e. it might be that there is no such a thing as a
-- "cheap" constant in a particularly involved instance, so always let
-- the Monad decide how to produce it.  At the very least, this allows
-- sharing.  It is absolutly necessary to have multipe t be associated
-- to a single m (e.g. base type + matrices).


class Monad m => Ring m t where
  add   :: t -> t -> m t
  sub   :: t -> t -> m t
  mul   :: t -> t -> m t
  neg   :: t -> m t

  zero  :: m t  -- additive neutral
  one   :: m t  -- multiplicative neutral 


-- Note that Rings are important because they can be structured
-- hierarchically, and are a way to structure computations.  Examples
-- of composite rings: Complex and Normal numbers, matrics and
-- polynomials.

-- FIXME: For composite rings it is currently not clear how to express
-- the operation of scaling by base ring (often field) scalars.  It
-- seems possible however to provide a lifting operation that maps
-- base ring scalars into field elements, and then use the ring's
-- multiplication.  Maybe that is more appropriate.  It works in
-- practice, but can possibly create a lot of spurious "times zero"
-- terms in generated code.  One thing thoug: many compositive rings
-- support a reducing operation such as a norm or a determinant, that
-- reduces an element to a scalar, often to be used as a denominator
-- in a division of the base field.

-- The problem is: should we add the base ring type in the definition
-- of a Ring?  It might help type inferences, and we're going to need
-- it anyway.



-- It is important to keep Ring structure and differentiable structure
-- separate.  Rings can be used to define composite rings: 



-- Composite rings (FIXME: scale by base ring? (real or complex numbers)




-- However, we're also going to need some non-linear functions to
-- build models that will then be linearized by automatic
-- differentiation.  It is OK to collect those in a single extra
-- class.

-- Once way to think about it is that transcendental functions are
-- always somehow defined at the meta level.  They are useful in code
-- (specification) but will be ephemeral in implementation (i.e. used
-- in autodiff at compile time to derive linearized update equations,
-- and replaced by polynomals or iterative systems in
-- implementations).

-- FIXME: Find a good name for this.  Differentiable?  Transcendental?

class Ring m t => Functions m t where
  div  :: t -> t -> m t
  sin  :: t -> m t
  cos  :: t -> m t
  exp  :: t -> m t
  sqrt :: t -> m t



-- Note that the algebraic structure does not put any constraints on
-- the monad.  We keep that in here because all practical instances
-- will be monadic.

-- These are use all over the place
op1 :: (Traversable f, Zip f, Monad m) => (a -> m z)      -> (f a ->        m (f z))
op2 :: (Traversable f, Zip f, Monad m) => (a -> b -> m z) -> (f a -> f b -> m (f z))
op1 f a   = sequence $ fmap f a       -- which is just traverse
op2 f a b = sequence $ zipWith f a b  -- why is there no traverse2 ?


-- Use short names for Complex and Dual numbers as they are used
-- frequently in construction and matching.
                              
-- Note that using representable functors will avoid needing to define
-- Zip instance.  For now it seems concrete is better.  I'm already
-- having trouble fitting the larger design together, and really, it
-- is only a single line.

-- Complex numbers.
--
data C t = C t t deriving (Show, Functor, Foldable, Traversable)
instance Zip C where zip (C ar ai) (C br bi) = C (ar,br) (ai,bi)

instance Ring m t => Ring m (C t) where
  add = op2 add
  sub = op2 sub
  neg = op1 neg
  mul (C ar ai) (C br bi) = do
    ar_br <- mul ar br ; ai_bi <- mul ai bi ; a <- sub ar_br ai_bi
    ar_bi <- mul ar bi ; ai_br <- mul ai br ; b <- add ar_bi ai_br
    return $ C a b
  zero = do zero' <- zero ;               return $ C zero' zero'
  one  = do zero' <- zero ; one' <- one ; return $ C one'  zero'

instance (Ring m t, Functions m t) => Functions m (C t) where
  exp (C ar ai) = do
    r <- exp ar
    rc <- mul r =<< cos ai
    rs <- mul r =<< sin ai
    return $ C rc rs
  sin = undefined
  cos = undefined
  div = undefined
  sqrt = undefined


-- Dual numbers.
--
-- This is a monadic version of:
-- http://conal.net/blog/posts/what-is-automatic-differentiation-and-why-does-it-work
--
data D t = D t t deriving (Show, Functor, Foldable, Traversable)
instance Zip D where zip (D a da) (D b db) = D (a,b) (da,db)

instance Ring m t => Ring m (D t) where
  add = op2 add
  sub = op2 sub
  neg = op1 neg
  mul (D a da) (D b db) = do
    c    <- mul a b
    a_db <- mul a db ; b_da <- mul b da ; dc <- add a_db b_da
    return $ D c dc
  zero = do c0 <- zero ;             return $ D c0 c0
  one  = do c0 <- zero ; c1 <- one ; return $ D c1 c0


instance (Ring m t, Functions m t) => Functions m (D t) where
  exp (D a da) = do
    expa  <- exp a
    dexpa <- mul da expa
    return $ D expa dexpa
  sin (D a da) = do
    sina  <- sin a
    dsina <- mul da =<< neg =<< cos a
    return $ D sina dsina
  cos (D a da) = do
    cosa  <- cos a
    dcosa <- mul da =<< sin a
    return $ D cosa dcosa

  div = undefined
  sqrt = undefined
  


--  exp    (D x x') = D (exp    x) (x' * exp x)
--  log    (D x x') = D (log    x) (x' / x)
--  sqrt   (D x x') = D (sqrt   x) (x' / (2 * sqrt x))
--  sin    (D x x') = D (sin    x) (x' * cos x)
--  cos    (D x x') = D (cos    x) (x' * (- sin x))
--  asin   (D x x') = D (asin   x) (x' / sqrt (1 - sqr x))
--  acos   (D x x') = D (acos   x) (x' / (-  sqrt (1 - sqr x)))






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

poing :: Ring m t => [t] -> m ([t], t)
poing [r, i] = do
  phasor@(C (_ :: t) _) <- one  -- Use actual numbers?
  C r' i' <- mul phasor (C r i)
  return ([r', i'], r')

 



