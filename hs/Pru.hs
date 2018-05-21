{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Pru where
import StateCont
import Data.Map (Map, (!), empty)
import qualified Data.Map as Map



class Monad m => Pru m where
  label :: m Label
  jmp :: Label -> m ()
  op2 :: Opc2 -> Op -> Op -> m ()
  

data Op = Reg String      deriving Show
data Opc2 = Mov           deriving Show
data Label = Label String deriving Show
data Ins = Op2 Opc2 Op Op
         | Jmp Label      deriving Show

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

  op2 opc a b = do
    putStrLn $ show $ Op2 opc a b
  jmp l =
    putStrLn $ show $ Jmp l
  

-- Trace: show register trace

-- Central idea is to use a state-continuation monad that can be
-- evaluated to a concrete trace.  Simplify traces to differentially
-- encoded register updates.

type Trace   = [(Delay,RegName,RegVal)]
type Delay   = Int
type RegName = String
type RegVal  = Int

type TraceM = SC State Trace  -- monad
type State = (Regs, Trace)    -- threaded state
type Regs = Map RegName RegVal

trace :: TraceM () -> Trace
trace (SC sc) = sc s0 k where
  k (_, trace) () = trace  -- top level continuation
  s0 = (regs0, [])         -- initial state
  regs0 = empty

instance Pru TraceM where

  label = do
    return $ Label "label"

  -- I find it more instructive to just spell this out completely,
  -- writing the primitives in CPS as explicit state-continuation
  -- operations.
  
  op2 Mov (Reg dst) (Reg src) = SC sc where
    sc (regs, trace) k = k s' () where
      val    = regs ! src
      regs'  = Map.insert dst val regs
      delay  = 1
      trace' = (delay, dst, val):trace  -- strict?
      s'     = (regs', trace')
      
  jmp _ =
    return ()


  
  

  
