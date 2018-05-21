module StateCont(SC(..),Comp,Cont) where

import Control.Applicative
import Control.Monad

-- State Continuation monad.

-- v: monad param = return value of CPS fragment, passed to continuation
-- s: extra state, traveling along side CPS values
-- r: concrete return value of computation

-- The continuation takes a state s and a value v and produces a value
-- r.  Note that the type r comes from the "root" continuation: the
-- end point of a whole CPS computations.  It can be largely ignored.
type Cont  s r v = s -> v -> r

-- A CPS computation fragment takes a state s and a continuation Cont
-- and produces a final result r (by invokation of a continuation).
type Comp  s r v = s -> Cont s r v -> r


-- Untagged bind & return.  
bindSC :: Comp s r v -> (v -> Comp s r v') -> Comp s r v'
bindSC  c f = c' where
  c' s k = c s (\s' v -> (f v) s' k)

returnSC :: v -> Comp s r v
returnSC v = c where
  c s k = k s v
  

-- Wrap in a tagged type to create Monad instance.
data SC  s r v = SC {cpsComp :: Comp s r v}
instance Functor (SC s r) where
  fmap = liftM
instance Applicative (SC s r) where
  pure = return
  (<*>) = ap
instance Monad (SC s r) where
  return v       =   SC (returnSC v)
  (SC c) >>= f   =   SC (c `bindSC` (cpsComp . f))
