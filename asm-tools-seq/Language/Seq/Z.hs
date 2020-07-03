-- FIXME: I'm mixing z and z^-1 forms.  It will work out eventually.
-- Just getting rid of bad intuitions.

-- Z and D transforms.
--
-- Note this uses the "flipped", i.e. generating function
-- representation.  It gets rid of all the negative exponents which is
-- notationally very inconvenient.  I do not understand how people
-- tolerate it.  Filters live in the past.  Accept it!
--
-- When dealing with causal signals and filters, it makes much more
-- sense to use the positvie exponent convention.  This means that
-- poles/zeros are flipped across the unit circle: stable filters have
-- poles outside of the unit circle.  Thanks to Adhemar Bultheel for
-- showing me the light.
--
-- The D and Z transforms relate by the substitution
--
--     d = z - 1
--     z = 1 + d
--
-- The D transform is a representation that makes sense for
-- oversampled filters which usually have poles and zeros concentrated
-- in the vicinity of z == 1.  For those filters, s =~ d, where s is
-- the parameter of the flipped Laplace transform.
--
-- Highly oversampled filters that are implemented in the d domain are
-- more numerically stable, at the expense of the extra subtraction
-- necessary to implement the d operation using a delay and a
-- subtraction.
--


-- 1. PLOT
--
-- Convert update functions expressed in z (delay) or d (difference)
-- form to their respective transforms.  Let's do this first in the z
-- domain.
--
-- Assume the transfer function can be expressed in state space form
--
--     x = A z x + B i
--     o = C x   + D i
--
-- Where x is the state variable, z is the delay operator, making z^-1
-- the forward operator.  i is the input vector and o is the output
-- vector.
--
-- Given i, the state solution is x = (Iz - A)^-1 B i
--
-- To compute the transfer function in terms of frequency, we need to
-- map frequency to z and compute the inverse, or solve for x in the
-- linear set of equations:
--
--     (Iz - A) x = B i
--

-- Now I want both to just compute this numerically, but I also want
-- to generate a functon that can perform the plot as part of a gui
-- that runs on a simple device without Haskell.  Leave that for
-- later, and first solve a system.

-- I wonder if it makes sense to use d transform to then avoid having
-- to worry about bad numerical condition?



-- https://hackage.haskell.org/package/hmatrix
-- http://dis.um.es/~alberto/hmatrix/hmatrix.html

