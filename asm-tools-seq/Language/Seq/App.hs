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

module Language.Seq.App where

import Language.Seq(Seq,S)
import Language.Seq.Lib(lift1,lift2,lift3)
import qualified Language.Seq as Seq
import qualified Language.Seq.Lib as SeqLib

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad


-- The ensure sharing, make sure values pass through bind in some way,
-- e.g. using a let/bind-like construct:

var :: forall m r t. Seq m r => m (r S) -> (m (r S) -> m t) -> m t
var mv f = mv >>= \v -> f $ return v

square :: Seq m r => m (r S) -> m (r S)
square x' = var x' $ \x -> x `mul` x

-- or point-free operators and tuples

dup :: Seq m r => m (r S) -> m (r S, r S)
dup = fmap $ \a -> (a, a)

uncur :: Seq m r => (m (r S) -> m (r S) -> m (r S)) -> m (r S, r S) -> m (r S)
uncur f mp = f (fmap fst mp) (fmap snd mp)

square' :: Seq m r => m (r S) -> m (r S)
square' = (uncur mul) . dup




-- The rest of the language can then be represented as "pure"
-- operations on m (r S).

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

-- I went over this a couple of times and got really confused.  Here
-- are some dead ends.

-- a)
--
-- The faulty intuition that "just adding another bind and return
-- pair" will solve the sharing issue.  Clearly that is not the case
-- because return is an identity: 'm >>= return' is the same as 'm'

-- b)
--
-- I attempted to still use do notation and handle cases like this:
--
--    a ->   a -> m a
--    a -> m a -> m a
--  m a ->   a -> m a
--  m a -> m a -> m a
--
-- One solution is to solve it at the syntax level, which definitely
-- seems like a valid way to work (essentially convert to ANF / do
-- notation), but requires a lot of infrastructure.

-- The other solution to use type classes to perform the a / m a
-- selection turns out to be not very useful.  Intermediate nodes are
-- ambiguous, requiring a lot of type annotation.  See git for old
-- versions.

-- c)
--
-- See haskell-cafe reply by Sebastiaan Joosten on 2/5/20:
-- https://mail.haskell.org/pipermail/haskell-cafe/2020-February/131882.html
--
-- {-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
-- class Pureable a b where
--   maybePure :: a -> b
-- instance Pureable a [a] where
--   maybePure = pure
-- instance Pureable [a] [a] where
--   maybePure = id

-- (.&&) :: (Pureable a [Bool], Pureable b [Bool])
--       => a -> b -> [Bool]
-- a .&& b = (&&) <$> maybePure a <*> maybePure b

-- test :: [Bool] -> [Bool]
-- test x = (True .&& x) .&& (False .&& x)
