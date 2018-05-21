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
  block :: Label -> m () -> m ()
  jmp :: Label -> m ()
  op2 :: Opc2 -> Op -> Op -> m ()
  

data Op = Reg String      deriving Show
data Opc2 = Mov           deriving Show
data Label = Label Int    deriving Show
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
  label     = return $ Label "FIXME_unique" 
  block l b = print l >> b
  op2 o a b = print $ Op2 o a b
  jmp l     = print $ Jmp l
  


-- Block / Trace is a two-pass compiler.
-- Pass1: construct a dictionary of basic blocks
-- Pass2: execute a trace

-- A basic block

-- Pass1 is constructed through a 


-- Trace: show register trace

-- Central idea is to use a state-continuation monad that can be
-- evaluated to a concrete trace.  Simplify traces to differentially
-- encoded register updates.

-- 

type Trace   = [(Delay,RegName,RegVal)]
type Delay   = Int
type RegName = String
type RegVal  = Int

type TraceM = SC State Blocks   -- monad
type State  = (Label, Labels)   -- threaded state
type Regs   = Map RegName RegVal
type Labels = (Label, Block)
type Label  = Int

trace :: TraceM () -> Trace
trace (SC sc) = sc s0 k where
  k (_, trace) () = trace  -- top level continuation
  s0 = (regs0, [], [])     -- initial state
  regs0 = empty

instance Pru TraceM where

  -- I find it more instructive to just spell out each monadic
  -- operation as an explicit state continuation passing call: k s r
  
  -- Allocate a number, but do not evaluate the dictionary, so we can
  -- use knot-tying.
  label = SC sc where
    sc (regs, (n, labelMap), trace) k = k s' r where
      s' = (regs, (n+1, labelMap) trace)
      r = Label n

  -- Operations are deferred.
  
  op2 Mov (Reg dst) (Reg src) = SC sc where
    sc (regs, labels, trace) k = k s' () where
      val    = regs ! src
      regs'  = Map.insert dst val regs
      delay  = 1
      trace' = (delay, dst, val):trace  -- strict?
      s'     = (regs', labels, trace')

  block _ b = b
      
  jmp (Label l) = SC sc where
    sc s k = k s () where
      (_, (_, labelMap, _), _) = s
      k' = 

      
      s' = (regs, labels', trace)
    
    return ()


  
  

  
