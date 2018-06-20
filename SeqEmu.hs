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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE DeriveTraversable #-}
--{-# LANGUAGE DeriveAnyClass #-}

module SeqEmu where
import Seq
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative
import Data.Map.Lazy (Map, (!), lookup, empty, insert, fromList)
import qualified Data.Map as Map
import Data.Bits
import Data.Functor.Compose
import Data.Key hiding((!))
import Prelude hiding(zipWith)
-- import Data.Functor.Apply
-- import Data.Functor.Rep

import Data.Dynamic
import Data.Typeable
-- import Unsafe.Coerce -- fixme

-- Main interpretation monad for Seq
newtype M t = M { runM :: ReaderT RegIn (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader RegIn,
   MonadState CompState)
type RegNum    = Int
type ConstVal  = Int
type RegVal    = Int
data RegType   = IntReg Size RegVal | ExtReg ExtState
-- Keep these two separate
type RegVals   = Map RegNum RegVal
type RegTypes  = Map RegNum RegType
-- More abstract, allows probing
type RegIn = RegNum -> RegVal
type CompState = (RegNum, RegTypes, RegVals, ExtStates)
type ExtStates = Map RegNum ExtState
type MemState = Map RegNum RegVal


-- External state threading.  The state type can be fully hidden as an
-- existential type, but the output needs to be dynamic so we can
-- recover it.
data ExtState = forall s. ExtState (s, s -> M (s, Dynamic))
class ExtStateOps s where



-- Primitive state manipulations.  Combine these with 'modify'
appRegNum    f (n, t, v, e) = (f n, t, v, e) ; getRegNum    = do (n, _, _, _) <- get ; return n
appTypes     f (n, t, v, e) = (n, f t, v, e) ; getTypes     = do (_, t, _, _) <- get ; return t
appVals      f (n, t, v, e) = (n, t, f v, e)
appExtStates f (n, t, v, e) = (n, t, v, f e) ; getExtStates = do (_, _, _, e) <- get ; return e

-- Similar, but monadic f, and specialized to one of the external state objects.
-- modifyExtState :: ()
modifyExtState r def f = do
  es <- getExtStates
  let e = Map.findWithDefault def r es
  (e', o) <- f e
  modify $ appExtStates $ insert r e'
  return o
  


data R t = R { unR :: Signal } -- phantom wrapper
data Signal = Reg Int
            | Val Size Int
type Size = Maybe Int


instance Seq M R where

  -- undriven signal
  signal (SInt sz r0) = do
    r <- makeRegNum
    modify $ appTypes $ insert r $ IntReg sz r0
    return $ R $ Reg  r

  constant (SInt sz r0) =
    R $ Val sz r0

  stype (R r) = do
    (sz, r0) <- styp r
    return $ SInt sz r0

  slice (R a) u l = do
    ((sa,_), va) <- val' a
    return $ R $ sliceVal (sa,va) u l

  -- driven signals
  op1 o (R a) = do
    ((sza,_), va) <- val' a
    return $ R $ truncVal sza $ f1 o va
  
  op2 o (R a) (R b) = do
    ((sza,_),va) <- val' a
    ((szb,_),vb) <- val' b
    return $ R $ case o of
      CONC ->
        concVal (sza, va) (szb, vb)
      _ ->
        truncVal (sz sza szb) $ f2 o va vb

  op3 o (R a) (R b) (R c) = do
    ((sza,_),va) <- val' a
    ((szb,_),vb) <- val' b
    ((szc,_),vc) <- val' c
    return $ R $ truncVal (sz sza (sz szb szc)) $ f3 o va vb vc

  -- register drive
  next (R (Reg a)) (R b) = do
    b' <- val b
    modify $ appVals $ insert' "SeqEmu.next: " a b'
    
  -- this is an artefact necessary for MyHDL non-registered outputs
  connect _ _ = error "SeqEmu does not support connect"



-- This can happen due to []'s applicative functor.
insert' tag = Map.insertWithKey err where
  err k v v_old = error $ tag ++ "double insert: (k,v,v_old)=" ++ show (k,v,v_old)
    
-- Value dereference & meta information.
val  = (fmap snd) . val'
val' (Val sz v) = return ((sz, v), v) -- Set reset value to actual value
val' (Reg r) = do
  v <- asks $ \regs -> regs r
  ts <- getTypes
  let IntReg sz r0 = ts ! r
  return ((sz, r0), v)

      
styp = (fmap fst) . val'



sz Nothing a = a
sz a Nothing = a
sz (Just a) (Just b) = Just $ max a b

truncVal Nothing v     = Val Nothing v
truncVal sz@(Just b) v = Val sz $ v .&. mask b

mask b = (shiftL 1 b) - 1

concVal (sza,va) (szb, vb) = 
  case szb of
    Nothing ->
      error "conc: rightmost argument needs defined size"
    Just szb' -> do
      let sz = liftA2 (+) sza szb in
        Val sz $ (shiftL va szb') .&. vb

-- Array slice.  This mimicks MyHDL's signal[upper:lower] construct.
-- Upper can be left unspecified as Nothing, which means the entire
-- signal is taken.
sliceVal (sa,va) upper lower = truncVal sa' va' where
  sa' = fmap (+ (- lower)) $ sz sa upper
  va' = shiftR lower va

f1 INV = complement

f2 ADD = (+)
f2 AND = (.&.)
f2 XOR = xor
f2 SLL = shiftL
f2 SLR = shiftR

f3 IF c a b = if c /= 0 then a else b

makeRegNum :: M Int  
makeRegNum = do
  n <- getRegNum
  modify $ appRegNum (+ 1)
  return n

-- Perform a single clock cycle.  Note that the state monad is used to
-- incrementally build up a dictionary of registers.  It is _not_ used
-- to thread register state from one machine tick to another.
runEmu :: M a -> RegIn -> ExtStates -> (a, RegTypes, RegVals, ExtStates)
runEmu m regsenv extsi = (v, regtypes, regso, extso) where
  state =  (0, Map.empty, Map.empty, extsi)
  (v, (_, regtypes, regso, extso)) = runState (runReaderT (runM m) regsenv) state

-- To perform a simulation, we need two things.  Both are built on runEmu.

-- a) The initial state.  Registers and their types and initial values
--    are embedded inside the program, as are external state
--    interfaces.  To get them out, we exectue the program once with
--    fake input.  External state is encoded explicitly.
reset :: M a -> (RegVals, ExtStates)
reset m = (r0, e0) where
  
  -- Run program once with fake input, to obtain just the type
  -- information.
  (_, types, _, _) = runEmu m (const 0) Map.empty

  -- Registers and external state are implemented differently, but are
  -- stored in the same type map.
  (regTypes, extTypes) = Map.partition isRegType types
  isRegType (IntReg _ _) = True
  isRegType (ExtReg _) = False

  r0 = Map.map initReg regTypes
  initReg (IntReg size initVal) = initVal

  e0 = Map.map initExt extTypes
  initExt (ExtReg v) = v
  

-- b) The register update function.  Two assumptions are made: an
--    arbitrary output is produced to allow user extension, along side
--    a "bus" of signals, which will be translated into a bus of
--    concrete Integers.
tick :: M a -> (RegVals, ExtStates) -> ((RegVals, ExtStates), a)
tick m (ri, ei) = ((ro, eo), o) where
  (o, _, ro, eo) = runEmu m (ri !) ei



-- The simluation is then the initialization and threading of the
-- update function.  Do it a little more general by allowing
-- user-provided state threading, which e.g. is used to implement
-- memories.

-- The first value in the output list corresponds to the reset state
-- of the registers and any combinatorial results computed from that.
-- The second value corresponds to the time instance associated to the
-- first active clock pulse, when registers are latched for the first
-- time.
ticks :: M o -> [o]
ticks m = t s0 where
  s0 = reset m -- probe with first state input
  t s  = o : t s' where
    (s', o) = tick m s


-- Special case: emulate input.
iticks :: Typeable o => (i -> M o) -> [i] -> [o]
iticks f is = ticks $ fixInput is f



-- Since we can't do anything with internal representations, these
-- functions are provided to convert to and from Int.  Signals can be
-- collected in a bus, represented by a Traversable.
probe :: Traversable f => f (R S) -> M (f Int)
probe = sequence . (fmap (val . unR))

-- Convenient special case for state threading.
probe' :: Traversable f => (t, f (R S)) -> M (t, f Int)
probe' (t,b) = fmap (t,) $ probe b

-- The reverse for inputs, but pure.
inject :: Functor f => (f Int) -> (f (R S))
inject = fmap (constant . (SInt Nothing))


-- Bind probe, inject to ticks, iticks.
trace :: Traversable f => M (f (R S)) -> [f Int]
trace mf = ticks $ mf >>= probe

itrace ::
  (Functor f, Traversable f', Typeable f, Typeable f') =>
  (f (R S) -> M (f' (R S))) -> [f Int] -> [f' Int]
itrace mf is = iticks mf' is' where
  mf' is = mf is >>= probe
  is' = map inject is


-- Generic external state threading
fixExtState :: Typeable o => (f s -> M (f s, o)) -> f s -> M o
fixExtState update init = do

  -- The state type can remain hidden, but state value and state
  -- update function need to be stored together to be able to perform
  -- the application.  The return value needs to come out again, so it
  -- is wrapped as Dynamic.
  let update' s = do
        (s', o) <- update s
        return (s', toDyn $ Just o)
      es0 = ExtState (init, update')
  
  -- Type and ExtState data are indexed by a unique register number.
  r <- makeRegNum
  
  -- Initial state is stored in the type dictionary.
  modify $ appTypes $ insert r $ ExtReg es0

  -- Compute update based on the current state.  Repack with update
  -- function to do the same next time.
  o <- modifyExtState r es0 $ \(ExtState (s, u)) -> do
    (s', o) <- u s
    return (ExtState (s', u), o)

  -- Recover the value.  Because r is unique, this is guaranteed to
  -- work as long as state is derived from the initial state as in
  -- 'ticks'.
  let Just o' = fromDyn o Nothing
  return o'



-- Implement memory as a threaded computation.
type Mem = (R S, R S, R S, R S) -> MemState -> M (MemState, R S)
mem :: Mem
mem (wEn, wAddr, wData, rAddr) memState = do
  
  -- Read
  rAddr' <- val $ unR rAddr 
  let rData' = Map.findWithDefault 0 rAddr' memState
  (SInt data_s _) <- stype wData
  let rData = constant $ SInt data_s rData'
  
  -- Write
  wAddr' <- val $ unR wAddr
  wData' <- val $ unR wData
  wEn'   <- val $ unR wEn
  let memState' =
        if wEn' == 0 then
          memState
        else
          insert wAddr' wData' memState
          
  return (memState', rData)


-- Couple memory access code to memory implementation.
-- Multiple memories are contained in a functor, similar to fixReg.
fixMemReg :: 
  (Zip f, Traversable f) =>
  f SType ->
  (f (R S) -> M (f (R S, R S, R S, R S), o)) ->
  f MemState -> M (f MemState, o)
fixMemReg t user s = fixReg t comb where

  -- Input/Output are named from memory's perspecitive: o is the
  -- memory's read port data register.  The memory enable, address and
  -- data input is implemented as a combinatorial network to provide
  -- single cycle read acces.
  comb o = do

    -- Apply user combinatorial network (o->i)
    -- x is just an output we pass along for generic threading.
    (i', x) <- user o

    -- Apply each memory's combinatorial network (i->o)
    so' <- sequence $ zipWith mem i' s
    let s' = fmap fst so'
        o' = fmap snd so'

    return (o', (s', x))

-- Tuck away memory state in an ExtState slot
fixMem :: 
  (Zip f, Traversable f, Typeable o) =>
  f SType -> (f (R S) -> M (f (R S, R S, R S, R S), o)) -> M o
fixMem t user = fixExtState update init where
  init   = fmap (\_ -> Map.empty) t
  update = fixMemReg t user


-- Tuck away input stream in an ExtState slot
fixInput :: 
  Typeable o =>
  [i] -> (i -> M o) -> M o
fixInput is f = fixExtState update is where
  update (i:is) = do
    o <- f i
    return (is, o)





-- For constants.
instance Num (R S) where
  fromInteger i = R $ Val Nothing $ fromInteger i
  -- Implement the rest just for constants.
  (+) = num2 (+)
  (*) = num2 (*)
  abs = num1 abs
  signum = num1 signum
  negate = num1 negate

-- Use Applicative?   Nope. not general enough.
num1 f (R (Val sza a)) =
  R $ Val sza $ f a
num2 f (R (Val sza a)) (R (Val szb b)) =
  R $ Val (sz sza szb) $ f a b






