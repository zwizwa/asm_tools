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
-- The former is used here, the latter in SeqApp.hs


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Language.Seq.Arr where

import Language.Seq(Seq,S)
import qualified Language.Seq as Seq
import qualified Language.Seq.Lib as SeqLib

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad





-- Some other observations.
--
-- . Seq is monadic, but the monadic structure is only needed for
--   metaprogramming.
--
-- . The structure we're modeling (circuits) is fixed.  Can this be
--   encoded in the types?
--
-- . Arrows should fix this.  But Arrows are awkward.
--
-- . Arrows are supposed to be equivalent to Category + Strong (Profuctor)
--
-- So let's try this and see if it buys anything, or if the Profunctor
-- bits are just spielerei.


-- It seems that the Kleisli Arrow is really the preferred approach,
-- as it allows the expression of sharing.

-- Instances:
-- Monad m => Category   (Kleisli m)
-- Monad m => Arrow      (Kleisli m)
-- Monad m => Profunctor (Kleisli m)
-- Monad m => Strong     (Kleisli m)

type SeqArr1 m r = Kleisli m (r S) (r S)
type SeqArr2 m r = Kleisli m (r S, r S) (r S)
type SeqArr3 m r = Kleisli m (r S, r S, r S) (r S)

uncurry3 f (a,b,c) = f a b c

integral :: Seq m r => SeqArr1 m r
integral = Kleisli SeqLib.integral

-- i2 :: Seq m r => SeqArr m r S S
-- i2 = integral . integral

add :: Seq m r => SeqArr2 m r
add = Kleisli $ uncurry Seq.add

sub :: Seq m r => SeqArr2 m r
sub = Kleisli $ uncurry Seq.sub

if' :: Seq m r => SeqArr3 m r
if' = Kleisli $ uncurry3 Seq.if'
