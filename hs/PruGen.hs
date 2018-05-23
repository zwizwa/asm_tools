-- FIXME: for now this just prints to stdout.
-- Will need at least a state monad to generate labels.

module PruGen where
import Pru

data Ins = Op2i Opc2i Reg Im
         | Op2r Opc2r Reg Reg
         | Op3 Opc3 Reg Reg Op
         | Jmp Label      
         | Label Label
         | Halt
         deriving Show


instance Pru IO where
  declare      = return 0 -- FIXME
  label l      = print $ Label l
  ins2i o a b  = print $ Op2i o a b
  ins2r o a b  = print $ Op2r o a b
  ins3 o a b c = print $ Op3 o a b c
  jmp l        = print $ Jmp l
  halt         = print $ Halt
