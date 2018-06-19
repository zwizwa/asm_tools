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


-- Main interpretation monad for Seq
newtype M t = M { runM :: ReaderT RegIn (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader RegIn,
   MonadState CompState)
type RegNum    = Int
type ConstVal  = Int
type RegVal    = Int
data RegType   = IntReg Size RegVal | IntMem
-- Keep these two separate
type RegVals   = Map RegNum RegVal
type RegTypes  = Map RegNum RegType
-- More abstract, allows probing
type RegIn = RegNum -> RegVal
type CompState = (RegNum, RegTypes, RegVals, Mems)
type Mems = Map RegNum MemState

-- EDIT: This is for later.  Make memories explicit.

-- To support composable state threading, the "value" of a register is
-- relaxed to an existential type.
data AbsReg = 
  forall s. (AbsRegOps s) => Signal { 
    stateInit   :: s, 
    stateUpdate :: s -> M s
  }
class AbsRegOps s where
  dumpAbsReg :: s -> String




-- Primitive state manipulations
appRegNum f (n, t, v, m) = (f n, t, v, m) ; getRegNum = do (n, _, _, _) <- get ; return n
appTypes  f (n, t, v, m) = (n, f t, v, m) ; getTypes  = do (_, t, _, _) <- get ; return t
appVals   f (n, t, v, m) = (n, t, f v, m)

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
runEmu :: M t -> RegIn -> (t, RegTypes, RegVals)
runEmu m i = (v, ts, so) where
  (v, (_, ts, so, _)) = runState (runReaderT (runM m) i) (0, Map.empty, Map.empty, Map.empty)

-- To perform a simulation, we need two things.  Both are built on runEmu.

-- a) The initial register state.  Registers and their types and
--    initial values are embedded inside the program.  To get them
--    out, we exectue the program once with all registeres set to 0.
reset :: M t -> Map RegNum RegVal
reset m = s0 where
  (_, types, _) = runEmu m $ const 0
  s0 = Map.map init types
  init (IntReg sz r0) = r0  -- FIXME: map

-- b) The register update function.  Two assumptions are made: an
--    arbitrary output is produced to allow user extension, along side
--    a "bus" of signals, which will be translated into a bus of
--    concrete Integers.
tick :: M t -> RegVals -> (RegVals, t)
tick m si = (so, o) where
  (o, _, so) = runEmu m (si !)


-- The simluation is then the initialization and threading of the
-- update function.  Do it a little more general by allowing
-- user-provided state threading, which e.g. is used to implement
-- memories.

-- The first value in the output list corresponds to the reset state
-- of the registers and any combinatorial results computed from that.
-- The second value corresponds to the time instance associated to the
-- first active clock pulse, when registers are latched for the first
-- time.
ticks :: (io -> M (io, t)) -> io -> [t]
ticks mf io0 = t io0 s0 where
  s0 = reset (mf io0) -- probe with first state input
  t io s  = o : t io' s' where
    (s', (io', o)) = f s
    f = tick $ mf io

-- Note: this is effectively a second state monad.  Currently it is
-- not very clear how to best abstract this:
-- a) explicit ticks function
-- b) parameterize M with extra state
-- c) use a custom monad stack, write M as a transformer



-- Special case: emulate input.
iticks :: (i -> M o) -> [i] -> [o]
iticks mf is0 = ticks mf' is0 where
  mf' (i:is) = fmap (is,) $ mf i


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




-- These are tied to the probe and inject functions.  They are a bit
-- clumsy.  I expect these to become trivial once a good runtime state
-- composition mechanism is in place.

traceSO :: Traversable f => (io -> M (io, f (R S))) -> io -> [f Int]
traceSO mf io0 = ticks mf' io0 where
  mf' io = mf io >>= probe'

traceIO :: (Functor f, Traversable f') => (f (R S) -> M (f' (R S))) -> [f Int] -> [f' Int]
traceIO mf is = iticks mf' is' where
  mf' is = mf is >>= probe
  is' = map inject is

traceO :: Traversable f => M (f (R S)) -> [f Int]
traceO m = traceSO (\() -> do o <- m; return ((), o)) ()


-- Implement memory as a threaded computation.
type MemState = Map RegNum RegVal
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
fixMem :: 
  (Zip f, Traversable f) =>
  f SType ->
  (f (R S) -> M (f (R S, R S, R S, R S), o)) ->
  f MemState -> M (f MemState, o)
fixMem t user s = fixReg t comb where

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


-- FIXME: conceptual error: The RegVal needs to be changed to
-- accomodate other things than Int.

-- -- Memory state is stored in a IntMem dictionary.
-- fixMem' :: 
--   (Zip f, Traversable f) =>
--   f SType -> (f (R S) -> M (f (R S, R S, R S, R S), o)) -> M o

-- fixMem' t user = fixReg t comb where

--   -- FIXME: It's weird to have these two phases intermixed:
--   -- default+type creation and readout.  For some reason they show up
--   -- here directly, making this obvious.  Maybe split this into
--   -- separate phases?
--   mems = sequence $ fmap mkMem t
--   mkMem _ = do
--     r <- makeRegNum
--     modify $ appTypes $ insert r $ IntMem Map.empty
--     IntMem memState <- asks $ \regs -> regs r
--     let memUpdate s' = modify $ appVals $ insert' "SeqEmu.fixMem: " r s'
--     return (memState, memUpdate)

--   -- Input/Output are named from memory's perspecitive: o is the
--   -- memory's read port data register.  The memory enable, address and
--   -- data input is implemented as a combinatorial network to provide
--   -- single cycle read acces.
--   comb o = do
--     mems' <- mems
--     let s = fmap fst mems'
--         u = fmap snd mems'

--     -- Apply user combinatorial network (o->i)
--     -- x is just an output we pass along for generic threading.
--     (i', x) <- user o

--     -- Apply each memory's combinatorial network (i->o)
--     so' <- sequence $ zipWith mem i' s
--     let s' = fmap fst so'
--         o' = fmap snd so'

--     sequence_ $ zipWith (.) u s'

--     return (o', x)







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






