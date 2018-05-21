{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pru where
import StateCont


class Monad m => Pru m where
  label :: m Label
  jmp :: Label -> m ()
  op2 :: (Op -> Op -> Ins) -> Op -> Op -> m ()
  

data Op = Reg String deriving Show
data Label = Label String deriving Show
data Ins
  = Mov Op Op
  | Jmp Label
  deriving Show

r10 = Reg "R10"
r11 = Reg "R11"

mov :: Pru m => Op -> Op -> m ()
mov = op2 Mov

-- As a convenience.  The explicit dereference is quite annoying when
-- the reference producer doesn't have any side effects.
o2 f a b = do
  a' <- a
  b' <- b
  f a' b'

-- Debug: print to console.
instance Pru IO where
  label = do
    let l = Label "label"
    putStrLn $ show l
    return l

  op2 ins a b = do
    putStrLn $ show $ ins a b
  jmp l =
    putStrLn $ show $ Jmp l
  

-- Trace: show register trace

-- Central idea is to use a state-continuation monad that can be
-- evaluated to a concrete trace.

type TraceM = SC State Trace  -- monad
type Trace = [String]         -- return value
data State = State Regs Trace -- threaded state
type Regs = [(String, Int)]

trace :: TraceM () -> Trace
trace (SC sc) = sc s0 k where
  k (State _ t) () = t  -- top level continuation
  s0 = State [] []   -- initial state


instance Pru TraceM where
  label = do
    return $ Label "label"
  op2 _ _ _ = do
    return ()
  jmp _ =
    return ()


  
  

  
