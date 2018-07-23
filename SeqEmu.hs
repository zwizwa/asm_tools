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

module SeqEmu where
import Seq
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Map.Lazy (Map, (!), lookup, empty, insert, fromList)
import qualified Data.Map as Map
import Data.Bits
import Data.Functor.Compose
import Data.Key(Zip(..),zipWith)
import Prelude hiding(zipWith)
import Data.Dynamic
import Data.Typeable
import Data.Maybe



-- Main interpretation monad for Seq
newtype M t = M { runM :: ReaderT (CompEnv R) (State CompState) t } deriving
  (Functor, Applicative, Monad,
   MonadReader (CompEnv R),
   MonadState CompState)
type RegNum    = Int
type RegVal    = Int
data StateType = IntReg SType | ProcessReg Process
data RegUpdate = RegUpdate
type CompEnv r = (Env r, RegIn)
-- Dictionaries
type RegVals    = Map RegNum RegVal
type StateTypes = Map RegNum StateType
type Processes  = Map RegNum Process
-- More abstract than concrete Map allows probing
type RegIn = RegNum -> RegVal
type CompState = (RegNum, StateTypes, RegVals, Processes)
-- Memory state implementation
type MemState = Map RegNum RegVal


-- See closeProcess.  Uses Dynamic types.  Too much hassle to make
-- this parametric.
data Process = Process Dynamic



-- Primitive state manipulations.  Combine these with 'modify'
appRegNum    f (n, t, v, e) = (f n, t, v, e) ; getRegNum    = do (n, _, _, _) <- get ; return n
appTypes     f (n, t, v, e) = (n, f t, v, e) ; getTypes     = do (_, t, _, _) <- get ; return t
appVals      f (n, t, v, e) = (n, t, f v, e)
appProcesses f (n, t, v, e) = (n, t, v, f e) ; getProcesses = do (_, _, _, e) <- get ; return e

data R t = R { unR :: Signal } -- phantom wrapper
data Signal = Reg Int
            | Val SType
            deriving Show
type Size = Maybe Int

fromRight' (Right a) = a
fromRight' (Left e) = error e

instance Seq M R where

  -- undriven signal
  signal (SInt sz r0) = do
    r <- makeRegNum
    modify $ appTypes $ insert r $ IntReg $ SInt sz r0
    return $ R $ Reg  r

  constant (SInt sz r0) =
    R $ Val $ SInt sz r0

  stype (R r) = styp r

  -- driven signals
  op1 o (R a) = do
    SInt sza va <- sval a
    let sz = op1size o sza
    return $ R $ truncVal sz $ f1 o va
  
  op2 o (R a) (R b) = do
    a'@(SInt sza va) <- sval a
    b'@(SInt szb vb) <- sval b
    -- Seq semantics determines size
    let sz = fromRight' $ op2size o sza szb
    return $ R $ truncVal sz $ case o of
      CONC -> conc' a' b'
      _    -> f2 o va vb

  op3 IF (R c) (R t) (R f) = do
    SInt szc vc <- sval c
    SInt szt vt <- sval t
    SInt szf vf <- sval f
    let sz = fromRight' $ op3size IF szc szt szf
        vr = if vc /= 0 then vt else vf
    return $ R $ truncVal sz vr

  slice (R a) u l = do
    v <- sval a
    return $ R $ sliceVal v u l

  -- register drive
  update (R (Reg a)) (R b) = do
    b' <- val b
    modify $ appVals $ insert' "SeqEmu.update: " a b'
    
  -- this is an artefact necessary for MyHDL non-registered outputs
  connect _ _ = error "SeqEmu does not support connect"

  -- see comments in Seq.erl
  getEnv = do
    (e,_) <- ask
    return e
  withEnv f m = do
    local (\(e,r) -> (f e,r)) m

  memory = memory'
  updateMemory = updateMemory'
  



-- This can happen due to []'s applicative functor.
insert' tag = Map.insertWithKey err where
  err k v v_old = error $ tag ++ "double insert: (k,v,v_old)=" ++ show (k,v,v_old)
    

-- Convert internal representation to current and reset values.
styp :: Signal -> M SType
sval :: Signal -> M SType

styp = (fmap fst) . sints
sval = (fmap snd) . sints
val v = do SInt _ v' <- sval v ; return v'

sints (Val i@(SInt sz v)) = do
  return $ (i, i)
sints (Reg r) = do
  v <- asks $ \(_,regs) -> regs r
  ts <- getTypes
  let IntReg v0@(SInt sz _) = ts ! r
  return $ (v0, SInt sz v)



combineSize a b = fromRight' $ combine a b


truncVal Nothing v     = Val $ SInt Nothing v
truncVal sz@(Just b) v = Val $ SInt sz $ v .&. ((shiftL 1 b) - 1)

conc' (SInt _ va) (SInt (Just szb) vb) = (shiftL va szb) .|. vb

-- Array slice.  This mimicks MyHDL's signal[upper:lower] construct.
-- Upper can be left unspecified as Nothing, which means the entire
-- signal is taken.
sliceVal :: SType -> Maybe Int -> Int -> Signal
sliceVal (SInt sa va) upper lower = truncVal sa' va' where
  va' = shiftR va lower
  sa'= fmap (+ (-lower)) upper

f1 INV = complement

f2 ADD = (+)
f2 SUB = (-)
f2 AND = (.&.)
f2 XOR = xor
f2 SLL = shiftL
f2 SLR = shiftR
f2 EQU = (\a b -> boolToInt $ a == b)
f2 op  = error $ "f2: " ++ show op


boolToInt True  = 1
boolToInt False = 0         
         
makeRegNum :: M Int  
makeRegNum = do
  n <- getRegNum
  modify $ appRegNum (+ 1)
  return n

-- Perform a single clock cycle.  Note that the state monad is used to
-- incrementally build up a dictionary of registers.  It is _not_ used
-- to thread register state from one machine tick to another.
runEmu :: M a -> RegIn -> Processes -> (a, StateTypes, RegVals, Processes)
runEmu m regsenv extsi = (v, regtypes, regso, extso) where
  state =  (0, Map.empty, Map.empty, extsi)
  (v, (_, regtypes, regso, extso)) =
    runState (runReaderT (runM m) (initEnv, regsenv)) state

-- To perform a simulation, we need two things.  Both are built on runEmu.

-- a) The initial state.  Registers and their types and initial values
--    are embedded inside the program, as are external state
--    interfaces.  To get them out, we exectue the program once with
--    fake input.  External state is encoded explicitly.
reset :: M a -> (RegVals, Processes)
reset m = (r0, e0) where
  
  -- Run program once with fake input, to obtain just the type
  -- information.
  (_, types, _, _) = runEmu m (const 0) Map.empty

  -- Registers and external state are implemented differently, but are
  -- stored in the same type map.
  (regTypes, extTypes) = Map.partition isIntReg types
  isIntReg (IntReg _) = True
  isIntReg (ProcessReg _) = False

  r0 = Map.map initReg regTypes
  initReg (IntReg (SInt size initVal)) = initVal

  -- Process initialization is performed in updateProcess when the
  -- state value is not present.
  e0 = Map.empty

-- b) The register update function.  Two assumptions are made: an
--    arbitrary output is produced to allow user extension, along side
--    a "bus" of signals, which will be translated into a bus of
--    concrete Integers.
tick :: M a -> (RegVals, Processes) -> ((RegVals, Processes), a)
tick m (ri, ei) = ((ro, eo), o) where
  (o, _, ro, eo) = runEmu m (ri !) ei



-- Simulation is then the the unfold reset + tick.

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

-- Input is implemented using extensible state threading...
iticks :: (Typeable i, Typeable o) => (i -> M o) -> [i] -> [o]
iticks f is = take' $ ticks $ closeInput is f where 
  take' = (map fromJust) . (takeWhile isJust) 

-- ... which is most useful when standard types are converted to Int.
-- For inputs, input bit sizes needs to be specified.
onInts ::
  (Zip i, Traversable o) =>
  i Int
  -> (i (R S) -> M (o (R S)))
  -> (i Int   -> M (o Int))
onInts bitSizes mod ints = do
  outs <- mod $ zipWith (\s i -> constant $ SInt (Just s) i) bitSizes ints
  sequence $ fmap (val . unR) outs





-- Generic external state threading.

-- Processes are similar to registers, except that they are completely
-- opaque.  State is containt in a map and threaded through 'ticks'.

toProcess   :: Typeable t => t -> Process
fromProcess :: Typeable t => Process -> t
toProcess   = Process . toDyn
fromProcess (Process p) =
  case fromDynamic p of
    Just v -> v
    Nothing -> error "fromProcess: Nothing"

process :: forall s. Typeable s => s -> M RegNum
process init = do
  r <- makeRegNum
  modify $ appTypes $ insert r $ ProcessReg $ toProcess init

  -- When running inside 'ticks', the first 'tick' or during 'probe',
  -- we'll have to initialize state.  This is a little quirky..
  mv <- (getProcess' r) :: M (Maybe s)
  case mv of
    Just _ ->
      return ()
    Nothing ->
      modify $ appProcesses $ insert r $ toProcess init
  
  return r

getProcess r = do
  Just v <- getProcess' r
  return v

getProcess' :: Typeable s => RegNum -> M (Maybe s)
getProcess' r = do
  ps <- getProcesses
  return $ case Map.lookup r ps of
    Nothing -> Nothing
    Just d -> Just $ fromProcess d

updateProcess :: Typeable s => RegNum -> (s -> M (s, o)) -> M o
updateProcess r update = do

  s <- getProcess r
  (s', o) <- update s
  modify $ appProcesses $ insert r $ toProcess s'
  return o

-- Bundle declaration and binding in the same way as closeReg, closeMem
closeProcess :: Typeable s => (s -> M (s, o)) -> s -> M o
closeProcess u i = do
  r <- process i
  updateProcess r u


-- closeProcess closes input "list popping" feedback loop.
closeInput :: 
  (Typeable i, Typeable o) =>
  [i] -> (i -> M o) -> M (Maybe o)

closeInput is f = closeProcess update is where
  update [] =
    return ([], Nothing)
  update (i:is) = do
    o <- f i
    return (is, Just o)


-- Memory is implemented using the "open" interface from Seq.erl

memory' :: SType -> M (R S, R Mem)
memory' td = do
  r <- process ((Map.empty, unR $ constant td) :: (MemState, Signal))
  (_, rd) <- (getProcess r) :: M (MemState, Signal)
  return (R rd, R $ Reg r)

updateMemory' :: R Mem -> (R S, R S, R S, R S) -> M ()
updateMemory' (R (Reg r)) memInputs = do
  (ms, _) <- getProcess r :: M (MemState, Signal)
  (ms', R rd') <- mem memInputs ms
  modify $ appProcesses $ insert r $ toProcess (ms', rd')
  return ()

-- MemState <-> register interface
type Memory = (R S, R S, R S, R S) -> MemState -> M (MemState, R S)
mem :: Memory
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







-- For constants.
instance Num (R S) where
  fromInteger i = R $ Val $ SInt Nothing $ fromInteger i
  -- Implement the rest just for constants.
  (+) = num2 (+)
  (*) = num2 (*)
  abs = num1 abs
  signum = num1 signum
  negate = num1 negate

-- Use Applicative?   Nope. not general enough.
num1 f (R (Val (SInt sza a))) =
  R $ Val $ SInt sza $ f a
num2 f (R (Val (SInt sza a))) (R (Val (SInt szb b))) =
  R $ Val $ SInt (combineSize sza szb) $ f a b





-- Streams

-- Insert extra spaces in between samples allowing custom tagging
-- (e.g. insert an enable signal in some form).

upSample :: (Bool -> a -> b) -> [Int] -> [a] -> [b]
upSample en spaces as = concat $ zipWith dup spaces as where
  dup 0 _ = []
  dup n a = (en True a) : (map (en False) $ replicate (n-1) a)

downSample :: (b -> Maybe a) -> [b] -> [a]
downSample sel = catMaybes . (map sel)


