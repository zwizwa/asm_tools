module Language.Grid.StateCont(SC(SC)) where


-- (copied from darcs/meta/dspm/StateCont.hs)

-- State Continuation monad.  I don't understand monad transformers
-- yet (FIXME) so this is implemented directly.

-- In order to make the types a bit manageble the code uses untagged
-- implementations of bind and return, and two type aliases to make
-- the type-spaghetti palatable.

-- v: monad param = return value of CPS fragment, passed to continuation
-- s: extra state, traveling along side CPS values
-- r: concrete return value of computation

-- The continuation takes a state s and a value v and produces a value
-- r annotated with th estate.  Note that the type r comes from the
-- "root" continuation: the end point of a whole CPS computations.
type Cont  s r v = s -> v -> r

-- A CPS computation fragment takes a state s and a continuation Cont
-- and produces a final result r (by invokation of a continuation).
type Comp  s r v = s -> Cont s r v -> r


-- Untagged bind & return.  
bindSC :: Comp s r v -> (v -> Comp s r v') -> Comp s r v'
bindSC  c f = c' where
  c' s k = c s k' where
    k' s' v = c'' s' k where
      c'' = f v

returnSC :: v -> Comp s r v
returnSC v = c where
  c s k = k s v
  

-- Wrap in a tagged type to create Monad instance.
data SC  s r v = SC {cpsComp :: Comp s r v}

returnSC' v      = SC (returnSC v)
bindSC' (SC c) f = SC (c `bindSC` (cpsComp . f))

instance Monad (SC s r) where
  return = returnSC'
  (>>=)  = bindSC'
 

-- It is now necessary to implement Functore and Applicative.  Since
-- we have explicit pure and bind ops, we just use those.
instance Functor (SC s r) where
  fmap f m = m `bindSC'` (\m' -> returnSC' $ f m')
    
instance Applicative (SC s r) where
  pure = returnSC'
  f <*> m = f `bindSC'` (\f' -> m `bindSC'` (\m' -> returnSC' $ f' m'))




