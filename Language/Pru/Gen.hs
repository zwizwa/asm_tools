
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Language.Pru.Gen(asm) where

import Language.Pru

import Control.Monad.State
import Control.Monad.Writer
import Data.List


-- Compile to intermediate Ins language using Writer+State monad.

type Gen = WriterT [Ins] (State Int)
data Ins = Ins String [O] | Lbl Int | Comment String deriving Show
data Asm = Asm [Ins]

asm :: Gen () -> Asm
asm m = Asm w where
  (((), w), s) = runState (runWriterT m) 0

w i = tell [i]

instance Pru Gen where
  declare           = do l <- get ; modify (+1) ; return $ L l
  label (L l)       = w $ Lbl l
  insri o a b       = w $ Ins (show o) [Reg a, Im b]
  insrr o a b       = w $ Ins (show o) [Reg a, Reg b]
  insrroo o a b c d = w $ Ins (show o) [Reg a, Reg b, c, d]
  insrro o a b c    = w $ Ins (show o) [Reg a, Reg b, c]
  insiri o a b c    = w $ Ins (show o) [Im a, Reg b, Im c]
  insiro o a b c    = w $ Ins (show o) [Im a, Reg b, c]
  insro o a b       = w $ Ins (show o) [Reg a, b]
  insi o i          = w $ Ins (show o) [Im i]
  inso o a          = w $ Ins (show o) [a]
  ins o             = w $ Ins (show o) []
  comment c         = w $ Comment c



-- Convert Ins language to concrete PRU ASM syntax

instance Show Asm where
  show (Asm lines) = concat $ map line lines

-- Handle special cases here.

line (Comment c)          = "\t;; " ++ c ++ "\n"
line (Lbl l)              = lbl l ++ ":\n"
line (Ins i [a,b,c,d])
  | i == "SBBO" || i == "LBBO" =
    let op' o@(Im _) = op o
        op' o@(Reg _) = error $ "FIXME: Pru.Gen.line needs special case for SBBO/LBBO register count"
    in "\t" ++ i ++ "\t" ++ (comma ["&" ++ op a, op b, op c, op' d]) ++ "\n"

line (Ins "XOUT" [a,b,c]) = "\tXOUT\t" ++ (comma [op a, "&" ++ op b, op c]) ++ "\n"
line (Ins opc ops)        = "\t" ++ opc ++ "\t" ++ (comma $ map op ops) ++ "\n"

lbl l = "L" ++ (show l)
op (Reg (R r)) = "R" ++ show r
op (Reg (Rw r w)) = "R" ++ show r ++ ".w" ++ show w
op (Reg (Rb r b)) = "R" ++ show r ++ ".b" ++ show b
op (Im (I i))  = show i
op (Im (L l))  = lbl l


comma = intercalate ", "
