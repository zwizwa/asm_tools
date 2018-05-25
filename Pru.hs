{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pru where

-- FIXME: Instructions are implemented as needed by the driving application.
class Monad m => Pru m where
  declare :: m I
  label   :: I -> m ()
  inso    :: OpcO   -> O           -> m ()
  insrr   :: OpcRR  -> R -> R      -> m ()
  insri   :: OpcRI  -> R -> I      -> m ()
  insro   :: OpcRO  -> R -> O      -> m ()
  insrro  :: OpcRRO -> R -> R -> O -> m ()
  insiri  :: OpcIRI -> I -> R -> I -> m ()
  ins     :: Opc -> m ()

  -- Instrumentation
  comment  :: String -> m () ; comment _  = return ()
  logStr   :: String -> m () ; logStr _   = return ()
  snapshot :: String -> m () ; snapshot _ = return ()

-- Operands
data O = Reg R | Im I                    deriving (Show,Eq,Ord)
data I = I Int | L Int                   deriving (Show,Eq,Ord)
data R = R Int | Rw Int Int | Rb Int Int deriving (Show,Eq,Ord)


-- Instructions
data OpcO   = JMP  deriving Show
data OpcRR  = MOV  deriving Show
data OpcRI  = LDI  deriving Show
data OpcRO  = JAL  deriving Show
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

jal :: Pru m => R -> O -> m ()
jal = insro JAL

jmp :: Pru m => O -> m ()
jmp = inso JMP

  


-- Shortcut in case no back-references are required.
label' :: Pru m => m I
label' = do
  l <- declare
  label l
  return l


nops :: forall m. Pru m => Int -> [m ()]
nops nb = replicate nb nop

