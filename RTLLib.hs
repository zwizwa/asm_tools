{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTLLib where
import RTL



-- Declarative register feedback operator: signal bundled with next
-- avoids the creation of non-driven signals, or multiple bindings
-- through next.
reg :: RTL m r => SType -> (r S -> m (r S)) -> m (r S)
reg t f = do
  r <- signal t -- create undriven signal
  reg' r f

-- Do expose the open version.  This does not have guarantees of reg,
-- but might be convenient as a building block.
reg' :: RTL m r => (r S) -> (r S -> m (r S)) -> m (r S)
reg' r f = do
  r' <- f r     -- create update equation with possible feedback
  next r r'     -- patch the register's input
  return r

inc :: RTL m r => r S -> m (r S)
inc c = int 1 >>= add c

counter :: RTL m r => SType -> m (r S)
counter t = reg t inc

delay :: RTL m r => r S -> m (r S)
delay x = do
  t <- stype x
  reg t $ \_ -> return x

edge d = do
  d0 <- delay d
  d `bxor` d0

bit b = do
  int 1 >>= band b


int :: forall m r. RTL m r => Int -> m (r S)
int v = constant (SInt Nothing v)

