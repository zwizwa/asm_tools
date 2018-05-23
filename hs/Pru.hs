{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Pru where

-- FIXME: Instructions are implemented as needed by the driving application.
class Monad m => Pru m where
  declare :: m Label
  label   :: Label -> m ()
  jmp     :: Label -> m ()
  ins2r   :: Opc2r -> Reg -> Reg       -> m ()
  ins2i   :: Opc2i -> Reg -> Im        -> m ()
  ins3    :: Opc3  -> Reg -> Reg -> Op -> m ()
  halt    :: m ()

-- Operands
data Op = Reg Reg | Im Im | Label Label
  deriving (Show, Eq, Ord)

-- Instructions
data Opc2r = MOV  deriving Show
data Opc2i = LDI  deriving Show
data Opc3  = ADD  deriving Show

-- No need for further wrapping.
type Reg   = Int
type Label = Int
type Im    = Int

-- Register file.  Spell these out as identifiers.  Maybe not necessary..
-- r0  = R  0 ; r1  = R  1 ; r2  = R  2 ; r3  = R 3
-- r4  = R  4 ; r5  = R  5 ; r6  = R  6 ; r7  = R 7
-- r8  = R  8 ; r9  = R  9 ; r10 = R 10 ; r11 = R 11
-- r12 = R 12 ; r13 = R 13 ; r14 = R 14 ; r15 = R 15
-- r16 = R 16 ; r17 = R 17 ; r18 = R 18 ; r19 = R 19
-- r20 = R 20 ; r21 = R 21 ; r22 = R 22 ; r23 = R 23
-- r24 = R 24 ; r25 = R 25 ; r26 = R 26 ; r27 = R 27
-- r28 = R 27 ; r29 = R 29 ; r30 = R 30 ; r31 = R 31


mov :: Pru m => Reg -> Reg -> m ()
mov = ins2r MOV

ldi :: Pru m => Reg -> Im -> m ()
ldi = ins2i LDI

add :: Pru m => Reg -> Reg -> Op -> m ()
add = ins3 ADD



-- Shortcut in case no back-references are required.
label' :: Pru m => m Label
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

  

