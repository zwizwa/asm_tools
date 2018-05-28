-- Emulation of sequential logic.

-- Note that Seq.hs only provides composition of signals and
-- operators, and uses Haskell as a macro language.  The main purpose
-- of this module is to provide test benches through 'trace'.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SeqEmu where
import Seq
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Map.Lazy (Map, (!), lookup, empty, insert, insertWith, fromList)
import qualified Data.Map as Map
import Data.Bits


-- Main interpretation monad for Seq
newtype M t = M { unM :: ReaderT RegIn (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader RegIn,
   MonadState CompState)
type RegNum   = Int
type ConstVal = Int
type RegVal   = Int
type RegType  = (Size, Int)
-- Keep these two separate
type RegVals  = Map RegNum RegVal
type RegTypes = Map RegNum RegType
-- More abstract, allows probing
type RegIn = RegNum -> RegVal
type CompState = (RegNum, RegTypes, RegVals)

-- Primitive state manipulations
appRegNum f (n,t,v) = (f n, t, v) ; getRegNum = do (n,_,_) <- get ; return n
appTypes  f (n,t,v) = (n, f t, v) ; getTypes  = do (_,t,_) <- get ; return t
appVals   f (n,t,v) = (n, t, f v)

data R t = R { unR :: Signal } -- phantom wrapper
data Signal = Reg Int
            | Val Size Int
type Size = Maybe Int


instance Seq M R where

  -- undriven signal
  signal (SInt sz r0) = do
    r <- makeRegNum
    modify $ appTypes $ insert r (sz, r0)
    return $ R $ Reg  r

  constant (SInt sz r0) = do
    return $ R $ Val sz r0

  stype (R r) = do
    (sz, r0) <- styp r
    return $ SInt sz r0

  -- driven signals
  op1 o (R a) = do
    ((sza,_), va) <- val' a
    return $ R $ truncVal sza $ f1 o va

  op2 o (R a) (R b) = do
    ((sza,_),va) <- val' a
    ((szb,_),vb) <- val' b
    return $ R $ truncVal (sz sza szb) $ f2 o va vb

  op3 o (R a) (R b) (R c) = do
    ((sza,_),va) <- val' a
    ((szb,_),vb) <- val' b
    ((szc,_),vc) <- val' c
    return $ R $ truncVal (sz sza (sz szb szc)) $ f3 o va vb vc

  -- register drive
  next (R (Reg a)) (R b) = do
    b' <- val b
    modify $ appVals $ insert a b'
    
  -- this is an artefact necessary for MyHDL non-registered outputs
  connect _ _ = error "SeqEmu does not support connect"
    
-- Value dereference & meta information.
val  = (fmap snd) . val'
val' (Val sz v) = return ((sz, v), v) -- Set reset value to actual value
val' (Reg r) = do
  v <- asks $ \regs -> regs r
  ts <- getTypes
  let (sz, r0) = ts ! r
  return ((sz, r0), v)
styp = (fmap fst) . val'

regval r regs = regs r


sz Nothing a = a
sz a Nothing = a
sz (Just a) (Just b) = Just $ max a b

truncVal Nothing v     = Val Nothing v
truncVal sz@(Just b) v = Val sz $ v .&. mask b

mask b = (shiftL 1 b) - 1

f1 INV = complement

f2 ADD = (+)
f2 AND = (.&.)
f2 XOR = xor
f2 SLL = shiftL
f2 SLR = shiftR

f3 IF c a b = if c /= 0 then a else b
  
makeRegNum = do
  n <- getRegNum
  modify $ appRegNum (+ 1)
  return n

runEmu m i = (v, ts, so) where
  (v, (_, ts, so)) = runState (runReaderT (unM m) i) (0, empty, empty)

-- While output register state is a map to allow post-processing,
-- input register state is represented more abstractly as a function
-- to allow probing.
stateIn si r = si ! r

-- The convention is that Seq programs we want to emulate will have
-- the type (M [R S]), which is rendered to [Int] when interpreted.
-- We call this Bus.
type Bus = [Int]
toBus rs = sequence $ map (val . unR) rs


-- Update function, including outputs
toTick :: M [R S] -> RegVals -> (RegVals, Bus)
toTick m si = (so, o) where
  (o, _, so) = runEmu (m >>= toBus) $ stateIn si

-- Same, but pass through an additional abstract output.
toTick' :: M (t, [R S]) -> RegVals -> (RegVals, (t, Bus))
toTick' m si = (so, (t, o)) where
  toOut (t, rs) = do is <- toBus rs; return (t, is)
  ((t, o), _, so) = runEmu (m >>= toOut) $ stateIn si




-- Without initial values encoding in types, we need to probe the
-- program to obtain them.  Ideally, initial state probing would be a
-- separate interpretation instance of Seq, but probing with 0 inputs
-- is adequate until it becomes a problem in future refactoring.
probe _ = 0

reset m = s0 where
  (_, types, _) = runEmu m probe
  s0 = Map.map init types
  init (sz,r0) = r0


-- Render signal waveforms from a monadic Seq.hs program
-- A simple example, but not general enough.  So name it with tick.
trace' :: M [R S] -> [Bus]
trace' m = t s0 where
  s0 = reset m
  f = toTick m
  t s = o : t s' where (s', o) = f s 

--- Same, but with abstract stateful external influence.  It's easier
--- to thread io manually here, as compared to extending the M state
--- to contain the extra type parameter.
traceIO :: io -> (io -> M (io, [R S])) -> [Bus]
traceIO io0 mf = t io0 s0 where
  s0 = reset (mf io0) -- probe with first state input
  t io s  = o : t io' s' where
    (s', (io', o)) = f s
    f = toTick' $ mf io

-- Implement input via traceIO by using io to contain the list.
trace :: ([R S] -> M [R S]) -> [Bus] -> [Bus]
trace mf is0 = traceIO is0 mf' where
  mf' (i:is) = do
    ri <- sequence [constant (SInt Nothing v) | v <- i]
    ro <- mf ri
    return (is, ro)
  
  


-- Memory.  It seems best to do this as a sort of coroutine that sits
-- in between two state updates.  For each memory:
-- a) reading registers read_addr, write_addr, write_data
-- b) send read_data as an input

-- Implement this by parameterizing trace'?  It seems simpler to use a
-- monadic operation concatenated to mf.  No.  Currently only works
-- with state extension in trace' loop + custom update.

-- When going that route, it seems that inputs can also be handled as
-- some kind of register interface.  I.e. do it in one place, not two.





