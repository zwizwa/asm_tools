{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Pru where

-- FIXME: Instructions are implemented as needed by the driving application.
class Monad m => Pru m where
  declare :: m L
  label   :: L -> m ()
  jmp     :: L -> m ()
  ins2r   :: Opc2r -> R -> R      -> m ()
  ins2i   :: Opc2i -> R -> I      -> m ()
  ins3    :: Opc3  -> R -> R -> O -> m ()
  halt    :: m ()

-- Operands
data O = Reg R | Im I | Label L deriving (Show, Eq, Ord)
data R = R Int | Rw Int Int | Rb Int Int deriving (Show,Eq,Ord)
type I = Int
type L = Int

-- Instructions
data Opc2r = MOV  deriving Show
data Opc2i = LDI  deriving Show
data Opc3  = ADD | CLR | SET
  deriving Show


mov :: Pru m => R -> R -> m ()
mov = ins2r MOV

ldi :: Pru m => R -> I -> m ()
ldi = ins2i LDI

add :: Pru m => R -> R -> O -> m ()
add = ins3 ADD

clr :: Pru m => R -> R -> O -> m ()
clr = ins3 CLR

set :: Pru m => R -> R -> O -> m ()
set = ins3 SET

-- Shortcut in case no back-references are required.
label' :: Pru m => m L
label' = do
  l <- declare
  label l
  return l


-- -- As a convenience.  The explicit dereference is quite annoying when
-- -- the reference producer doesn't have any side effects.
-- o2 f a b = do
--   a' <- a
--   b' <- b
--   f a' b'

  

