{-# LANGUAGE ScopedTypeVariables #-}

module Language.Pru.Lib where

import Language.Pru

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
-- 0,1,2 need to be separate
wait _ 0 = return ()
wait _ 1 = nop
wait _ 2 = nop >> nop
-- 3,4 are done as nops also, because the routine below won't be any
-- shorter.  Explicit nops are more readable in the output.
wait _ 3 = nop >> nop >> nop
wait _ 4 = nop >> nop >> nop >> nop
-- Create a wait loop with possible 1-nop preroll
wait reg cycles = nops where
  cycles' = cycles - 1              -- account for ldi loop init
  (loops, extra) = divMod cycles' 2 -- div accounts for loop length
  nops = do
    comment $ "{ PruLib.wait " ++ show cycles
    wait  reg extra
    loop  reg loops
    comment $ "}"
  loop reg loops = do  
    ldi reg (I loops)        -- 1
    l <- label'
    sub reg reg (Im (I 1))   -- 1
    qbne l  reg (Im (I 0))   -- 1


-- Put an abstract data type here to allow easy extension for
-- different compile-time predicate types.
data Pred = IfBit R O Bool

notPred (IfBit reg bit val) = IfBit reg bit $ not val

-- conditional move if bit is set (balanced, 3 cycles)
cmov :: Pru m => Pred -> R -> R -> m ()
cmov pred dst src  = do
  when pred           $ mov dst src
  when (notPred pred) $ nop

-- Conditional do if bit is set/unset
-- 1 instruction if false, 1 + n instructions if true
when :: Pru m => Pred -> m () -> m ()
when (IfBit reg bit pol) block = do
  skip <- declare
  case pol of
    True  -> qbbc skip reg bit
    False -> qbbs skip reg bit
  block
  label skip
