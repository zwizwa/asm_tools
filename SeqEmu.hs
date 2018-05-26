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

runEmu m i = runState (runReaderT (unM m) i) (0, empty, empty)


-- Update function without outputs
toTick' m si = so where
  (_, (_, _, so)) = runEmu m $ stateIn si

stateIn si r = si ! r

-- Update function, including outputs
toTick :: M [R S] -> RegVals -> (RegVals, [Int])
toTick m si = (so, o) where
  (o', (_, _, so)) = runEmu m $ stateIn si
  o = map val o'
  val (R (Val _ v)) = v
  val (R (Reg r)) = so ! r


-- Probe to obtain initial values.
-- Ideally, probe is a separate type.  But 0 works fine.
reset m = s0 where
  probe _ = 0
  (_, (_, types, _)) = runEmu m probe
  s0 = Map.map init types
  init (sz,r0) = r0


-- Render signal waveforms from a monadic Seq.hs program
type Bus = [Int]
trace :: M [R S] -> [Bus]
trace m = t s0 where
  s0 = reset m
  f = toTick m
  t s = o : t s' where (s', o) = f s 

-- Same, but with input.
trace' :: ([R S] -> M [R S]) -> [Bus] -> [Bus]
trace' mf is@(i0:_) = t s0 is where
  s0 = reset (mo i0) -- probe with first input
  mo i = mi >>= mf where
    mi = sequence $ [constant (SInt Nothing v) | v <- i]
  t s (i:is)  = o : t s' is where
    (s', o) = f s
    f = toTick $ mo i
    


    
