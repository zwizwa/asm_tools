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
  insrr   :: OpcRR  -> R -> R      -> m ()
  insri   :: OpcRI  -> R -> I      -> m ()
  insrro  :: OpcRRO -> R -> R -> O -> m ()
  insiri  :: OpcIRI -> I -> R -> I -> m ()
  ins     :: Opc -> m ()
  comment :: String -> m ()

-- Operands
data O = Reg R | Im I | Label L deriving (Show, Eq, Ord)
data R = R Int | Rw Int Int | Rb Int Int deriving (Show,Eq,Ord)
type I = Int
type L = Int

-- Instructions
data OpcRR  = MOV  deriving Show
data OpcRI  = LDI  deriving Show
data OpcIRI = XOUT deriving Show
data Opc    = NOP | HALT deriving Show
data OpcRRO = ADD | CLR | SET deriving Show


mov :: Pru m => R -> R -> m ()
mov = insrr MOV

ldi :: Pru m => R -> I -> m ()
ldi = insri LDI

add :: Pru m => R -> R -> O -> m ()
add = insrro ADD

clr :: Pru m => R -> R -> O -> m ()
clr = insrro CLR

set :: Pru m => R -> R -> O -> m ()
set = insrro SET

xout :: Pru m => I -> R -> I -> m()
xout = insiri XOUT

halt :: Pru m => m ()
halt = ins HALT

nop :: Pru m => m ()
nop = ins NOP
  


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

  

