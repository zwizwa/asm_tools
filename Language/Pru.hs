{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Pru where

-- FIXME: Instructions are implemented as needed by the driving application.
class Monad m => Pru m where
  declare :: m I
  label   :: I -> m ()
  inso    :: OpcO    -> O                -> m ()
  insi    :: OpcI    -> I                -> m ()
  insrr   :: OpcRR   -> R -> R           -> m ()
  insri   :: OpcRI   -> R -> I           -> m ()
  insro   :: OpcRO   -> R -> O           -> m ()
  insrro  :: OpcRRO  -> R -> R -> O      -> m ()
  insiri  :: OpcIRI  -> I -> R -> I      -> m ()
  insiro  :: OpcIRO  -> I -> R -> O      -> m ()
  insrroo :: OpcRROO -> R -> R -> O -> O -> m ()
  ins     :: Opc -> m ()

  -- Meta
  comment  :: String -> m () ; comment _  = return ()


-- Operands
data O = Reg R | Im I                    deriving (Show,Eq,Ord)
data I = I Int | L Int                   deriving (Show,Eq,Ord)
data R = R Int | Rw Int Int | Rb Int Int deriving (Show,Eq,Ord)


-- Instructions
data OpcO    = JMP  deriving Show
data OpcRR   = MOV  deriving Show
data OpcRI   = LDI  deriving Show
data OpcRO   = JAL  deriving Show
data OpcIRI  = XOUT deriving Show
data Opc     = NOP | HALT deriving Show
data OpcRRO  = ADD | SUB | CLR | SET deriving Show
data OpcIRO  = QBGT | QBGE | QBLT | QBLE | QBEQ | QBNE | QBBS | QBBC deriving Show
data OpcRROO = SBBO | LBBO deriving Show
data OpcI    = QBA deriving Show

mov :: Pru m => R -> R -> m ()
mov = insrr MOV

ldi :: Pru m => R -> I -> m ()
ldi = insri LDI

add :: Pru m => R -> R -> O -> m ()
add = insrro ADD

sub :: Pru m => R -> R -> O -> m ()
sub = insrro SUB

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

qbgt :: Pru m => I -> R -> O -> m ()
qbgt = insiro QBGT

qbge :: Pru m => I -> R -> O -> m ()
qbge = insiro QBGE

qblt :: Pru m => I -> R -> O -> m ()
qblt = insiro QBLT

qble :: Pru m => I -> R -> O -> m ()
qble = insiro QBLE

qbeq :: Pru m => I -> R -> O -> m ()
qbeq = insiro QBEQ

qbne :: Pru m => I -> R -> O -> m ()
qbne = insiro QBNE

qbbs :: Pru m => I -> R -> O -> m ()
qbbs = insiro QBBS

qbbc :: Pru m => I -> R -> O -> m ()
qbbc = insiro QBBC

qba :: Pru m => I -> m ()
qba = insi QBA

-- These are quirky.  
-- Semantics:
--
-- - LBBO &REG1, Rn2, OP(255), IM(124)
--   memcpy(offset(REG1), Rn2+OP(255), IM(124))
--
-- - LBBO &REG1, Rn2, OP(255), bn
--   memcpy(offset(REG1), Rn2+OP(255), R0.bn)
--
-- Note: the latter is currently not implemented properly

lbbo :: Pru m => R -> R -> O -> O -> m ()
sbbo :: Pru m => R -> R -> O -> O -> m ()
lbbo = insrroo LBBO
sbbo = insrroo SBBO


-- Shortcut in case no back-references are required.
label' :: Pru m => m I
label' = do
  l <- declare
  label l
  return l


nops :: forall m. Pru m => Int -> [m ()]
nops nb = replicate nb nop

