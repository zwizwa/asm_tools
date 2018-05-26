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
newtype M t = M { unM :: ReaderT RegEnv (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader RegEnv,
   MonadState CompState)
type Inits = [(RegNum, Size, Int)]
type RegNum = Int
type ConstVal = Int
type RegVal = (Size, Int, Maybe Int)
type RegEnv = RegNum -> RegVal
type RegMap = Map RegNum RegVal
type CompState = (RegNum, RegMap)

-- Primitive state manipulations
appRegNum f (n,c) = (f n, c) ; getRegNum = do (n,_) <- get ; return n
appOut    f (n,c) = (n, f c) 

data R t = R { unR :: Signal } -- phantom wrapper
data Signal = Reg Int
            | Val Size Int
type Size = Maybe Int



instance Seq M R where

  -- undriven signal
  signal (SInt sz r0) = do
    r <- makeRegNum
    modify $ appOut $ insert r (sz, r0, Nothing)
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
    let f (sz, r0, Nothing) = (sz, r0, Just b')
    modify $ appOut $ Map.adjust f a
    
  -- this is an artefact necessary for MyHDL non-registered outputs
  connect _ _ = error "SeqEmu does not support connect"
    
-- Value dereference & meta information.
val  = (fmap snd) . val'
val' (Val sz v) = return ((sz, v), v) -- Set reset value to actual value
val' (Reg r) = do (sz, r0, Just v) <- asks (regval r) ; return ((sz, r0), v)
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

runEmu m i = runState (runReaderT (unM m) i) (0, empty)


-- Find update function.
compileUpdate m si = so where
  (_, (_, so)) = runEmu m $ stateIn si

stateIn si r = si ! r

-- Same, but require a signal list as monadic value.
compileUpdate' :: M [R S] -> RegMap -> (RegMap, [Int])
compileUpdate' m si = (so, o') where
  (o, (_, so)) = runEmu m $ stateIn si
  o' = map val o
  val (R (Val _ v)) = v
  val (R (Reg r)) = v where (_,_,Just v) = so ! r

-- Initial values are recorded by the Writer so we can collect them.
compileInit m = s0 where
  fakeState _ = (Nothing, 0, Just 0)
  (_, (_, s)) = runEmu m fakeState
  s0 = Map.map kill s
  kill (sz,r0,_) = (sz,r0, Just r0)



-- Final product gives enough information to produce an infinite sequence.
compile  m = (compileInit m, compileUpdate  m)
compile' m = (compileInit m, compileUpdate' m)


-- 'trace' computes signal waveforms from a monadic Seq.hs program
type Bus = [Int]
trace :: M [R S] -> [Bus]
trace m = t s0 where
  (s0, f) = compile' m
  t s = o : t s' where (s', o) = f s 

-- -- Trace with inputs.
-- trace' :: ([R S] -> M [R S]) -> [Bus] -> [Bus]
-- trace' mf = t s0 where
--   s0 = compileInit (mi' >>= mf)
--   mi' = sequence $ cycle [constant (SInt Nothing 0)] -- FIXME: won't work
--   t s (i:is)  = o : t s' is where
--     (s', o) = f i s
--     f i = f' where
--       f' = compileUpdate' m'
--       m' = mi >>= mf
--       mi = sequence $ map input i
--       input v = constant (SInt Nothing v)
    


    
