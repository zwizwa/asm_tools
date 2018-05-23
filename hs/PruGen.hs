-- FIXME: for now this just prints to stdout.
-- Will need at least a state monad to generate labels.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module PruGen(asm) where
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Pru

data Ins = Ins String [Op]
         | Lbl Label
         deriving Show

type Gen = WriterT [Ins] (State Int)

instance Pru Gen where
  declare      = do l <- get ; modify (+1) ; return l
  label l      = write $ Lbl l
  ins2i o a b  = write $ Ins (show o) [Reg a, Im b]
  ins2r o a b  = write $ Ins (show o) [Reg a, Reg b]
  ins3 o a b c = write $ Ins (show o) [Reg a, Reg b, c]
  jmp l        = write $ Ins "JMP" [Label l]
  halt         = write $ Ins "HALT" []


write ins = tell [ins]

asm :: Gen () -> Asm
asm m = Asm w where
  (((), w), s) = runState (runWriterT m) 0

data Asm = Asm [Ins]

instance Show Asm where
  show (Asm lines) = concat $ map line lines

line (Lbl l) = lbl l ++ ":\n"
line (Ins opc ops) = "\t" ++ opc ++ "\t" ++ (intercalate "," $ map op ops) ++ "\n"
lbl l = "L" ++ (show l)
op (Reg r) = "R" ++ show r
op (Im i)  = show i
op (Label l) = lbl l
