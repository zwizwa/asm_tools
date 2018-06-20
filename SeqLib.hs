{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SeqLib where
import Seq
import Control.Monad
import Control.Applicative


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

bit b = band b 1



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
case' ((cond, whenTrue):cases) dflt = do
  c <- cond
  t <- whenTrue
  f <- case' cases dflt
  if' c t f
  

-- Shift register in terms of slice + conc.
-- Return old and new for max flex.
shiftReg :: Seq m r => SType -> r S -> m (r S, r S)
shiftReg t i = do
  closeReg [t] $ \[r] -> do
    t' <- stype i
    let SInt (Just r_bits) _ = t
        SInt (Just i_bits) _ = t'
    r_drop  <- slice r (Just $ r_bits - i_bits) 0
    r_shift <- conc r_drop i
    return $ ([r_shift], (r, r_shift))


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


-- Lifted versions, to be able to use expressions.  Note this is not
-- the same as liftA2 because the operation is not pure.  It is a
-- multi-argument version of bind (=<<).  Using these seems to be a
-- bad idea. 
bindOp2 f a b = do a' <- a ; b' <- b ; f a' b'

add' :: Seq m r => (m (r S)) -> (m (r S)) -> (m (r S))
add' = bindOp2 add
