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


-- To allow for sharing, it is possible to play with curry/uncurry.
-- E.g. using fmapped versions of (,) fst snd it is possible to
-- implement any kind of sharing, resulting in functions like:
uncurry ::
  Seq m r =>
  (m (r S) -> m (r S) -> m (r S)) ->
  m (r S, r S) -> m (r S)
uncurry f mp =
  f (fmap fst mp) (fmap snd mp)



-- The rest of the language can then be represented as:

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





-- Notes:

-- a) I went over this a couple of times and got really confused.
-- E.g. the faulty intuition that "just adding another bind and return
-- pair" will solve the issue.  Clearly that is not the case because
-- return is an identity: 'm >>= return' is the same as 'm'

-- b) I attempted to still use do notation and handle cases like this:
--
--    a ->   a -> m a
--    a -> m a -> m a
--  m a ->   a -> m a
--  m a -> m a -> m a
--
-- One solution is to solve it at the syntax level, which definitely
-- seems like a valid way to work (essentially convert to ANF / do
-- notation).

-- The other solution to use type classes to perform the a / m a
-- selection turns out to be not very useful.  Too ambiguous,
-- requiring a lot of type annotation.  See git for old versions.
