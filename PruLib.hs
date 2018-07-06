{-# LANGUAGE ScopedTypeVariables #-}

module PruLib where

import Pru

retR = (Rw 30 0) -- FIXME

call :: forall m. Pru m => I -> m ()
call l = jal retR (Im l)

ret :: forall m. Pru m => m ()
ret = jmp $ Reg $ retR

fun :: forall m. Pru m => m () -> m I
fun routine = do
  l <- label'
  routine ; ret
  return $ l


-- Cycle-accurate wait macro using only a counter register.
wait _ 0 = return ()
wait _ 1 = nop
wait _ 2 = nop >> nop
wait reg cycles = nops where
  cycles' = cycles - 1   -- account for ldi
  (counts, extra) = divMod cycles' 2
  nops = do
    comment $ "wait " ++ show cycles
    wait reg extra
    wait' reg counts
  wait' reg counts = do  
    ldi reg (I counts)       -- 1
    l <- label'
    sub reg reg (Im (I 1))   -- 1
    qbne l reg (Im (I 0))    -- 1

