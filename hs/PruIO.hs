module PruIO where

import Pru

-- Debug: print to console.
instance Pru IO where
  label     = return $ 0
  block l   = print l
  op2 o a b = print $ Op2 o a b
  jmp l     = print $ Jmp l
