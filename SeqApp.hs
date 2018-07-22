-- To further abstract, there seem to be two main avenues:
--
-- A) Kleisli Arrows
--
--     r a -> m ( r b ),
--
-- B) "pure" Applicative interface
--
--     m ( r a ) -> m ( r b ).
--
--
-- The latter is used here, the latter in SeqArr.hs


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE IncoherentInstances #-}

module SeqApp where

import Seq(Seq,S)
import qualified Seq
import qualified SeqLib
import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad


-- There are two main ways I can think of to abstract composition.

-- A) Kleisli Arrows
--
--     r a -> m ( r b ),
--
-- B) "pure" Applicative interface
--
--     m ( r a ) -> m ( r b ).
--
-- The differences show up for multi-argument functions.  There, the
-- latter interface cannot express sharing.

-- For more information, see SeqArr.hs



-- EDIT: Jury is still out on this.  It seems that the core idea is
-- the Kleisli arrow, not m a -> m b.  The latter has sharing issues.


-- The base language is monadic.  Operations follow the pattern:
--
--   r S -> m (r S)
--   r S -> r S -> m (r S)
--   ...
--
-- However, this can be a little awkward to work with, so define an
-- applicative interface as well, lifting the primitives to:
--
--   m (r S) -> m (r S)
--   m (r S) -> m (r S) -> m (r S)
--   ...
--
-- While convenient, note that this cannot implement sharing
-- E.g. for a 2-argument f, the application 'f m m' will duplicate the
-- circuit needed to compute m.
--
-- Note: I keep coming back to the faulty intuition that "just adding
-- another bind and return pair" will solve the issue.  Clearly that
-- is not the case because return is an identity:
-- 'm >>= return' is the same as 'm'

-- So, operations are provided for convenience in the hope that the
-- sharing issue is understood.  The rule of thumb is that if there is
-- fanout, you should funnel composition through a binding operation.



type SeqApp1 m r = (m (r S)) -> (m (r S))
type SeqApp2 m r = (m (r S)) -> (m (r S)) -> (m (r S))
type SeqApp3 m r = (m (r S)) -> (m (r S)) -> (m (r S)) -> (m (r S))

inv  :: Seq m r => SeqApp1 m r

add  :: Seq m r => SeqApp2 m r
mul  :: Seq m r => SeqApp2 m r
equ  :: Seq m r => SeqApp2 m r
band :: Seq m r => SeqApp2 m r
bxor :: Seq m r => SeqApp2 m r
bor  :: Seq m r => SeqApp2 m r
sll  :: Seq m r => SeqApp2 m r
slr  :: Seq m r => SeqApp2 m r
conc :: Seq m r => SeqApp2 m r

if'  :: Seq m r => SeqApp3 m r

-- Tese are different from fmap, liftA2 because the primitive
-- operations are monadic.
lift1 f ma       = do a <- ma                     ; f a
lift2 f ma mb    = do a <- ma ; b <- mb           ; f a b
lift3 f ma mb mc = do a <- ma ; b <- mb ; c <- mc ; f a b c

inv = lift1 Seq.inv

add  = lift2 Seq.add
mul  = lift2 Seq.mul
equ  = lift2 Seq.equ
band = lift2 Seq.band
bxor = lift2 Seq.bxor
bor  = lift2 Seq.bor
sll  = lift2 Seq.sll
slr  = lift2 Seq.slr
conc = lift2 Seq.conc

if'  = lift3 Seq.if'

-- The applicative interface allows overriding.
instance Seq m r => Num (m (r S)) where
  (+) = add
  (*) = mul
  fromInteger n = return $ Seq.constant $ Seq.SInt Nothing $ fromInteger n
  negate = inv
  abs    = error $ "TODO: abs"
  signum = error $ "TODO: signum"


-- The above is not the whole story.
-- The real issue is that a couple of interfaces are needed:
--
--    a ->   a -> m a
--    a -> m a -> m a
--  m a ->   a -> m a
--  m a -> m a -> m a
--
-- One solution is to solve it at the syntax level.  E.g. create a
-- 'lisp' on top of this.  Take s-expressions, perform macro
-- substitution, and reduce to ANF before mapping onto the monadic
-- form.
--b
-- Another solution is to use some a converter type class.  This turns
-- out to be very awkward to use.  I did not find a way to properly
-- constrain the conversions, requiring type annotations of the
-- intermediaries.  Leaving it here for reference.

-- -- Convert any type to a monadic representation of a signal.
-- -- See SeqApp.hs
-- class Seq m r => SeqMRS m r t where
--   seqMR :: t -> m (r S)




-- -- The main use for this is in applicative interfaces.  See SeqApp.hs
-- instance forall m r t. Seq m r => SeqMRS m r (m (r S)) where seqMR = id  
-- instance forall m r t. Seq m r => SeqMRS m r    (r S)  where seqMR = return

-- lift1 f a   = do a' <- seqMR a                 ; f a'
-- lift2 f a b = do a' <- seqMR a ; b' <- seqMR b ; f a' b'

-- inv :: SeqMRS m r a => a -> m (r S)
-- inv = lift1 Seq.inv

-- add :: forall m r a b. (SeqMRS m r a, SeqMRS m r b) => a -> b -> m (r S)
-- sub :: forall m r a b. (SeqMRS m r a, SeqMRS m r b) => a -> b -> m (r S)

-- add = lift2 Seq.add
-- sub = lift2 Seq.sub

-- -- However it seems this will need quite a bit of type annotation at
-- -- the user end to constrain the types.  E.g. to use these in monadic
-- -- form works fine, and actually seems to remove the need for
-- -- annotation altogether:

-- f a = do
--   b <- add a a
--   c <- add b b
--   return c

-- -- But this will not work without constraining the intermediates.
-- -- Can this be solved somehow?
-- f' :: forall m r a. SeqMRS m r a => a ->  m (r S)
-- f' a = a `add` ((a `add` a) :: m (r S))
-- -- f' a = a `add` a `add` a

-- -- The whole thing seems like a bad idea...
