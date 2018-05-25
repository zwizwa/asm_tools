{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RTL where


  
data Signal = S Int | L Int deriving Show

data Op2 = ADD deriving Show
data Op1 = INV deriving Show

-- Semantics of signals:
-- 1) exactly one driver
-- 2) from comb creates a wire that cannot have loops
-- 3) from seq creates a register which can create loops across clock ticks

class Monad m => RTL m where

  op2 :: Op2 -> Signal -> Signal -> m Signal
  op1 :: Op1 -> Signal -> m Signal
  
  set :: Signal -> Signal -> m ()

add :: forall m. RTL m => Signal -> Signal -> m Signal
add = op2 ADD

inv :: forall m. RTL m => Signal -> m Signal
inv = op1 INV

