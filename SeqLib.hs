{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}

module SeqLib where
import Seq
import Control.Monad
import Control.Applicative
import Data.Key(Zip(..),zipWith)
import Prelude hiding (zipWith)
import Data.Bits hiding (bit)

-- Shortcuts for common type constructions.
bits :: Int -> SType
bits' n = SInt (Just n)
bits n = bits' n 0

bit = bits 1
bit' = bits' 1

-- Special closeReg case: single register, with register as output
reg :: Seq m r => SType -> (r S -> m (r S)) -> m (r S)
reg t f = do closeReg [t] $ \[r] -> do r' <- f r ; return ([r'], r)


-- Some simple building blocks

inc :: Seq m r => r S -> m (r S)
inc c = add c 1

counter :: Seq m r => SType -> m (r S)
counter t = reg t inc

delay :: Seq m r => r S -> m (r S)
delay x = do
  t <- stype x
  reg t $ \_ -> return x

edge d = do
  d0 <- delay d
  d `bxor` d0

-- bit b = band b 1



-- A test of completeness is to implement a clock synchronizer.
-- Simplify it to power-of-two division.

-- Combinatorial part
sync' :: forall m r. Seq m r => r S -> r S -> r S -> m (r S)
sync' s0 i s = do
  e  <- edge i   -- edge detector on input
  s' <- inc s    -- default is free running counter
  if' e s0 s'    -- conditional reset on edge
  
-- Bound to register
sync :: Seq m r => SType -> r S -> m (r S)
sync t i = do
  reg t $ sync' (constant t) i

-- Note: we only support the combinatorial (dual-clause) if.
case' :: Seq m r => [(m (r S), m (r S))] -> m (r S) -> m (r S)
case' [] dflt = dflt
case' ((cond, whenTrue):clauses) dflt = do
  c <- cond
  t <- whenTrue
  f <- case' clauses dflt
  if' c t f
  

-- Shift register in terms of slice + conc.
-- Return old and new for max flex.
shiftReg :: Seq m r => SType -> r S -> m (r S, r S)
shiftReg tr i = do
  closeReg [tr] $ \[r] -> do
    r_shift <- shiftUpdate r i
    return $ ([r_shift], (r, r_shift))

-- Inner routine is useful without feedback.
shiftUpdate r i = do
  tr <- stype r
  ti <- stype i
  let SInt (Just r_bits) _ = tr
      SInt (Just i_bits) _ = ti
  r_drop  <- slice r (Just $ r_bits - i_bits) 0
  r_shift <- conc r_drop i
  return r_shift
  


integral :: Seq m r => r S -> m (r S)
integral x = do
  t <- stype x
  reg t $ add x


-- Multi-argument versions
sum :: Seq m r => [r S] -> m (r S)
sum = reduce ADD

reduce :: Seq m r => Op2 -> [r S] -> m (r S)
reduce ADD [] = return 0
reduce MUL [] = return 1
reduce XOR [] = return 0
reduce AND [] = return $ -1
reduce OR  [] = return 0
reduce opc [] = error $ "reduce: no identity element for " ++ show opc
reduce opc [a] = return a
reduce opc [a,b]  = (op2 opc) a b
reduce opc (a:as) = (reduce opc as) >>= (op2 opc) a




-- Applicative interface.

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


type SeqMOp1 m r = (m (r S)) -> (m (r S))
type SeqMOp2 m r = (m (r S)) -> (m (r S)) -> (m (r S))

inv'  :: Seq m r => SeqMOp1 m r

add'  :: Seq m r => SeqMOp2 m r
mul'  :: Seq m r => SeqMOp2 m r
equ'  :: Seq m r => SeqMOp2 m r
band' :: Seq m r => SeqMOp2 m r
bxor' :: Seq m r => SeqMOp2 m r
bor'  :: Seq m r => SeqMOp2 m r
sll'  :: Seq m r => SeqMOp2 m r
slr'  :: Seq m r => SeqMOp2 m r
conc' :: Seq m r => SeqMOp2 m r

-- Tese are different from fmap, liftA2 because the primitive
-- operations are monadic.
lift1 f ma    = do a <- ma           ; f a
lift2 f ma mb = do a <- ma ; b <- mb ; f a b

inv' = lift1 inv

add'  = lift2 add
mul'  = lift2 mul
equ'  = lift2 equ
band' = lift2 band
bxor' = lift2 bxor
bor'  = lift2 bor
sll'  = lift2 sll
slr'  = lift2 slr
conc' = lift2 conc


-- The applicative interface allows overriding.
instance Seq m r => Num (m (r S)) where
  (+) = add'
  (*) = mul'
  fromInteger n = return $ constant $ SInt Nothing $ fromInteger n
  negate = inv'
  abs    = error $ "TODO: abs"
  signum = error $ "TODO: signum"
  

-- Note that this is not the whole story.
-- The real issue is that a couple of interfaces are needed:
--
--    a ->   a -> m a
--    a -> m a -> m a
--  m a ->   a -> m a
--  m a -> m a -> m a
--
-- So not clear how to make that into anything useful.  It seems the
-- real solution is to use surface syntax that desugars into ANF.
--
-- So maybe create a 'lisp' on top of this.  Take s-expressions,
-- perform macro substitution, and reduce to ANF before mapping onto
-- the monadic form.





-- Note that Seq does not support "imperative" conditionals.  To
-- implement this kind of behavior, use a Traversable Zip container
-- (e.g. []).  This make the parallel muxing behavior explicit.

ifs :: (Seq m r, Traversable f, Zip f) => r S -> f (r S) -> f (r S) -> m (f (r S))
ifs c t f = sequence $ zipWith (if' c) t f

-- This makes me think that case statements might be implemented as
-- multiplexers as well?  does this need special care, or is it easy
-- for the synthesizer to recover the structure?  e.g. MyHDL does
-- perform some magic for "case" statements I believe.

-- Fixme: implement case' in terms of the more general cases.
cases ::
  (Seq m r, Traversable f, Zip f)
  => [(m (r S), m (f (r S)))]
  -> m (f (r S))
  -> m (f (r S))
cases [] dflt = dflt
cases ((cond, whenTrue):clauses) dflt = do
  c <- cond
  t <- whenTrue
  f <- cases clauses dflt
  ifs c t f


log2 i = f 0 where
  f n = case shiftL 1 n >= i of
    True -> n
    False -> f $ n + 1
    
t_log2 (SInt (Just n_bits) _) =  SInt (Just $ log2 n_bits) 0
t_log2 _ = SInt Nothing 0  


-- Streams

-- RTL is associated to streams by collecting the output of registers,
-- one for each clock cycle.

-- In practice, this is too limiting.  It is necessary to represent
-- streams at arbitrary rate.  This can be done by accompanying a
-- stream at clock rate, with a stream of enable bits, and using the
-- enable bits as a "synchronous clock".

-- For now it is ok to use partially applied pairs for this, as it
-- gives the necessary instances.  Maybe call these "tagged streams" ?
type Stream r = (,) r S


-- Building block for converting bit streams to word streams.
clocked_shift :: Seq m r => SType -> (r S, r S) -> m (r S, r S)
clocked_shift t_sr@(SInt (Just nb_bits) _) (bitClock, bitVal) = do
  let t_sr' = t_log2 t_sr
      n_max = constant $ SInt Nothing $ nb_bits - 1
  (wordClock', wordVal) <-
    closeRegEn bitClock [t_sr, t_sr'] $
    \[sr,n] -> do
      wc  <- n `equ` n_max
      n1  <- inc n
      n'  <- if' wc 0 n1
      sr' <- shiftUpdate sr bitVal
      return ([sr',n'], (wc, sr'))
  wordClock <- bitClock `band` bitClock
  return (wordClock, wordVal)


-- TEST.  Todo: how to make it possible to put a test right next to an
-- implementation?  Maybe not a good idea as it pulls in a lot of
-- dependencies.

test_clocked_shift = ()
    


-- UART.
--
-- Some observations:
-- a) Factored into timing generation + shift register
-- b) Timing can be factored using an enable signal

-- The part below only computes timing. 

-- Detect start bit, go to "on" state and produce a number of pulses.
-- wait for stop bit and produce parallel output strobe.

-- ( Personal note: One of the hardest things to unlearn going from
-- CPU programming to hardware programming is that conditionals do not
-- "save work".  For this an instruction sequencer with conditional
-- branches is needed.  Conditionals in HDL are static multiplexers. )

  
async_receiver ::
  forall m r. Seq m r =>
  SType -> r S -> m (r S)
async_receiver t@(SInt (Just n_bits) _) i = sm where
  sm = closeReg [bit, t, t'] update
  t' = t_log2 t
  
  update s@[is_on, n, sr] = do
    n'  <- inc n
    sr' <- shiftUpdate sr i
    
    -- switch on when i->0
    off <- ifs i s [1,1,0,0]
    on  <- cases
           [(i `equ` 8, return [0,0,0,0])]
           (return [1, 1, n', sr'])

    (o:s'@[_,_,_]) <- ifs is_on on off
    return (s',o)


-- FIXME: enable should probably be explicit.  If used only to slow
-- things down, the output strobes will not be 1-bit width.  A simple
-- way to fix that is to encode strobes as alternations.

-- Still not really done with figuring out how to decompose this.  But
-- it is clear that the hard part is "controllers", "sequencers":
-- decompose a circuit into basic building blocks, and the network
-- that controls the enables, strobes, muxes, etc..










