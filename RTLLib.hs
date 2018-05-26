{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTLLib where
import RTL



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

