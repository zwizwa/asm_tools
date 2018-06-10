{-# LANGUAGE ScopedTypeVariables #-}

module PruLib where

import Pru

ret :: forall m. Pru m => m ()
ret = jmp $ Reg $ Rw 30 0

sub :: forall m. Pru m => m () -> m (m ())
sub routine = do
  l <- label'
  routine ; ret
  return $ jal (Rw 30 0) (Im l)
