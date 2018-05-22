-- FIXME: for now this just prints to stdout.
-- Will need at least a state monad to generate labels.

module PruGen where
import Pru

data Ins = Op2 Ins2 Register Op
         | Op3 Ins3 Register Register Op
         | Jmp Label      
         | Label Label
         | Halt
         deriving Show


instance Pru IO where
  declare      = return 0 -- FIXME
  label l      = print $ Label l
  ins2 o a b   = print $ Op2 o a b
  ins3 o a b c = print $ Op3 o a b c
  jmp l        = print $ Jmp l
  halt         = print $ Halt
