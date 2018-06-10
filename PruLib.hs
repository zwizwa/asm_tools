{-# LANGUAGE ScopedTypeVariables #-}

module PruLib where

import Pru

retR = (Rw 30 0) -- FIXME

call :: forall m. Pru m => I -> m ()
call l = jal retR (Im l)

ret :: forall m. Pru m => m ()
ret = jmp $ Reg $ retR

sub :: forall m. Pru m => m () -> m I
sub routine = do
  l <- label'
  routine ; ret
  return $ l
