
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module PruGen(asm) where
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Pru

-- Compile to intermediate Ins language using Writer+State monad.

type Gen = WriterT [Ins] (State Int)
data Ins = Ins String [O] | Lbl L | Comment String deriving Show
data Asm = Asm [Ins]

asm :: Gen () -> Asm
asm m = Asm w where
  (((), w), s) = runState (runWriterT m) 0

w i = tell [i]

instance Pru Gen where
  declare        = do l <- get ; modify (+1) ; return l
  label l        = w $ Lbl l
  insri o a b    = w $ Ins (show o) [Reg a, Im b]
  insrr o a b    = w $ Ins (show o) [Reg a, Reg b]
  insrro o a b c = w $ Ins (show o) [Reg a, Reg b, c]
  insiri o a b c = w $ Ins (show o) [Im a, Reg b, Im c]
  jmp l          = w $ Ins "JMP" [Label l]
  ins o          = w $ Ins (show o) []
  comment c      = w $ Comment c



-- Convert Ins language to concrete PRU ASM syntax

instance Show Asm where
  show (Asm lines) = concat $ map line lines

line (Comment c) = "\t;; " ++ c ++ "\n"
line (Lbl l) = lbl l ++ ":\n"
line (Ins opc ops) = "\t" ++ opc ++ "\t" ++ (intercalate ", " $ map op ops) ++ "\n"
lbl l = "L" ++ (show l)
op (Reg (R r)) = "R" ++ show r
op (Reg (Rw r w)) = "R" ++ show r ++ ".w" ++ show w
op (Reg (Rb r b)) = "R" ++ show r ++ ".b" ++ show b
op (Im i)  = show i
op (Label l) = lbl l


