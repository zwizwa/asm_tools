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


-- Main interpretation monad for Seq
newtype M t = M { unM :: ReaderT RegIn (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader RegIn,
   MonadState CompState)
type RegNum    = Int
type ConstVal  = Int
type RegVal    = Int
type RegType   = (Size, RegVal)
-- Keep these two separate
type RegVals   = Map RegNum RegVal
type RegTypes  = Map RegNum RegType
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

  constant (SInt sz r0) =
    R $ Val sz r0

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


-- This can happen due to []'s applicative functor.
insert' tag = Map.insertWithKey err where
  err k v v_old = error $ tag ++ "double insert: (k,v,v_old)=" ++ show (k,v,v_old)
    
-- Value dereference & meta information.
val  = (fmap snd) . val'
val' (Val sz v) = return ((sz, v), v) -- Set reset value to actual value
val' (Reg r) = do
  v <- asks $ \regs -> regs r
  ts <- getTypes
  let (sz, r0) = ts ! r
  return ((sz, r0), v)
styp = (fmap fst) . val'



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
  (v, (_, ts, so)) = runState (runReaderT (unM m) i) (0, Map.empty, Map.empty)

-- While output register state is a map to allow post-processing,
-- input register state is represented more abstractly as a function
-- to allow probing.
stateIn si r = si ! r

-- The convention is that Seq programs we want to emulate will have
-- the type (M (t, [R S])), which is rendered to (t, [Int]) when
-- interpreted.
type Bus = [Int]

-- The user-defined type t is there mostly to provide abstract
-- external state threading in traceIO, but might be useful for other
-- things.
toBus rs = sequence $ map (val . unR) rs
toOut (t, rs) = do is <- toBus rs; return (t, is)

-- Generic render function.  Produces concrete update function.
-- Here t is some other type threaded through the Monad.
-- FIXME: This can be further collapsed by exposing toOut.
toTick :: M (t, [R S]) -> RegVals -> (RegVals, (t, Bus))
toTick m si = (so, (t, o)) where
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

-- Render signal sequence produced by a state-parameterized Seq.hs
-- program. Abstract user state is threaded explicitly in this
-- function, which is simpler than extending the M state to contain
-- the extra type parameter.
traceState :: io -> (io -> M (io, [R S])) -> [Bus]
traceState io0 mf = t io0 s0 where
  s0 = reset (mf io0) -- probe with first state input
  t io s  = o : t io' s' where
    (s', (io', o)) = f s
    f = toTick $ mf io

-- Special case: Implement input via traceIO by using io to contain
-- the input list.
trace :: ([R S] -> M [R S]) -> [Bus] -> [Bus]
trace mf is0 = traceState is0 mf' where
  mf' (i:is) = do
    o <- mf [constant (SInt Nothing v) | v <- i]
    return (is, o)

-- Version without external state
trace' :: M [R S] -> [Bus]
trace' m = traceState () (\() -> do o <- m; return ((), o))


-- Implement memory as a threaded computation.
type MemState = Map RegNum RegVal
type Mem = MemState -> (R S, R S, R S, R S) -> M (MemState, R S)
mem :: Mem
mem memState (wEn, wAddr, wData, rAddr) = do

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
  (Applicative f, Traversable f) =>
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
    so' <- sequence $ liftA2 mem s i'
    let s' = fmap fst so'
        o' = fmap snd so'

    return (o', (s', x))



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
