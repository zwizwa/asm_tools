-- Given component semantics, convert a netlist to a function.

-- Use terminology used in circuit netlist:
-- . a pin is identified by a component id and a port id
-- . a net is a set of pins
-- . a netlist is a set of nets
-- . each component instance can be mapped to a behaviour/semantics

-- Note that this is different from "net" indicating network.  Also a
-- "netlist" isn't really a list, it is a set.



-- It is possible to convert a network to a function as long as
-- n-ports are functions.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module NetFun where

import Data.Map.Lazy (Map(..))
import Data.Set (Set(..))
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Foldable
import Data.Either
import Data.Maybe
-- import qualified Data.IntMap IntMap

-- A Fun is a map from input to output.
type Fun v = Map Port v -> Map Port v

-- Expected keys in the Fun input Map.  
type FunIn = Set Port

-- Note that the I/O signature is not encoded at the Haskell type
-- level to allow for easily parameterizable semantics, e.g. some
-- ports change between input and output based on a parameter.


-- Basic collection hierarchy.
type Netlist = Set Net
type Net     = Set Pin
type Pin     = (Component, Port)

-- Identifiers are implemented as strings.  When appropriate, the
-- context contains dictionaries that can resolve these to other
-- types.
type Port  = String
type Component = String

-- It is not necessary to define the component list seprately, as it
-- can be derived directly from the graph:
nports :: Netlist -> Set Component
nports = Set.map fst . Set.flatten



-- Meaning is created by mapping individual components to functions.
-- Later we would like to parameterize this, when components can have
-- multiple meanings, e.g. to implement I/O direction.
type Semantics v = Component -> (Fun v, FunIn)

-- The evaluation of a network can only be done relative to semantics.
-- But it is important to note that given a semantics, we can
-- "flatten" everything into a single function through signal
-- propagagion.  This is the main function we're interested in.

-- Evaluation can fail if the Netlist or Semantics are inconsistent.
-- Part of the responsibility of this code is to identify those
-- inconsistencies.

eval :: Show v => Semantics v -> Netlist -> Signals v -> Either String (Signals v)
type Signals v = Map Pin v


-- Configuration can be added later.  The idea is to be able to create
-- a family of interpretations.
type SemanticsFamily v = Config -> Semantics v
type Config = ()   



-- IMPLEMENTATION

-- The evaluation algorithm uses recursive refinement: evaluate when
-- all inputs to a component's semantics function are vailable, then
-- propagate the effect of the new values.

-- Take into account failure: our representation does not impose any
-- constraints on how function semantics can be assigned to prots, so
-- it is possible multiple drivers are present.

-- The monad stack uses State, Reader and Either.  Components:
type EvalState v = (Values v, InputWait, Signals v)
type EvalEnv v = (Semantics v, Nets, InDeps)

newtype M v t = M { runM :: ReaderT (EvalEnv v) (StateT (EvalState v) (Either String)) t } deriving
  (Functor, Applicative, Monad,
   MonadReader (EvalEnv v),
   MonadState (EvalState v))


-- The netlist is indexed with NetKey to allow some data structures to
-- be split up.
type Nets     = Map NetKey Net
type Values v = Map NetKey v
type NetKey   = Int

-- Besides the indexed netlist, the following data structures are used
-- to guide evaluation.

-- As net values gradually become available during evaluation, the Set
-- of inputs a component is waiting for will shrink.  When it becomes
-- empty, a component is ready to be evaluated.
type InputWait = Map Component (Set NetKey)

-- Once all inputs are available, the input :: (Map Port v) can be
-- constructed.  The following index data structure is used in that
-- process.  Note (Set Port) vs Port: it is possible that a Component
-- has multiple ports connected to the same net.
type InDeps = Map Component (Map NetKey (Set Port))


-- State and environemnt access

askNets   :: M v Nets
askSem    :: M v (Semantics v)
askInDeps :: M v InDeps

askNets   = do (_,nets',_)   <- ask ; return nets'
askSem    = do (sem',_,_)    <- ask ; return sem'
askInDeps = do (_,_,indeps') <- ask ; return indeps'

modifyNetVals   f = modify (\(vs,ws,os) -> (f vs, ws, os))
modifyInputWait f = modify (\(vs,ws,os) -> (vs, f ws, os))
modifyOutputs   f = modify (\(vs,ws,os) -> (vs, ws, f os))

getNetVals :: M v (Map Int v)
getNetVals = do (nv,_,_) <- get ; return nv

getInputWait :: M v InputWait
getInputWait = do (_,cs,_) <- get ; return cs


-- Main evaluation entry point.  See above for type.
eval semantics netlist ins = outs where

  -- Compute the initial state and environment.
  (allNets, inDeps, inputWait) = evalInit semantics netlist

  -- Recursively evaluate, starting at inputs
  env = (semantics, allNets, inDeps)
  initState = (Map.empty, inputWait, Map.empty)
  outs = case runStateT (runReaderT (runM $ drivePins ins) env) initState of
           Left msg ->
             Left msg
           Right ((), (_, _, os)) ->
             Right os

-- Mix semantics and netlist into data structures that are easier to use
-- during evaluation.

evalInit semantics netlist = (allNets, inDeps, inputWait) where
  
  -- Assign each net a unique NetKey, so we can split up the data
  -- structures.
  
  allNets :: Map NetKey Net
  allNets = Map.fromList $ zip [0..] $ Set.toList netlist

  -- The data structure that guides network recursion relates a
  -- Component to the inputs its Fun needs.  There are two versions of
  -- this: InDeps, which has additional Port information used to
  -- prepare the Fun's input type, and InputWait, used to
  -- incrementally keep track of whether all of a Component's input
  -- nets have a value.

  -- inputWait can be derivied directly from inDeps
  inputWait :: Map Component (Set NetKey)
  inputWait = Map.map Map.keysSet inDeps

  -- inDeps is an annotation of each component with its input nets,
  -- identified by the NetKey, and annotated with the ports connected
  -- to that net.
  
  inDeps :: Map Component (Map NetKey (Set Port))
  inDeps = Map.fromSet inDep allComps
  allComps = Set.map fst $ Set.flatten netlist 
  inDep comp = inNets where
    
    -- all of comp's pins
    inNets :: Map NetKey (Set Port)
    inNets = Map.filter (not . Set.null) $ Map.map inNet allNets

    -- ... for a single network
    inNet :: Net -> Set Port
    inNet = Set.flatten . Set.map inPin

    -- ... for a single pin in a network
    -- isomophic to :: Pin -> Maybe Port, but easier to compose
    inPin :: Pin -> Set Port
    inPin (c, p) = case (c == comp) && (Set.member p inPorts) of
      True  -> Set.singleton p
      False -> Set.empty
    (_, inPorts) = semantics comp





drivePins :: forall v. Show v => Signals v -> M v ()
drivePins i = m where
  m = do
    sequence_ $ map setPin $ Map.toList i
    return ()
    
setPin (pin, val) = do

  netKey <- pinNet pin
  netVals <- getNetVals
  
  case Map.lookup netKey netVals of
    
    Just val' ->
      -- Something is wired wrong if the net already has a value.
      fail $ "pin already has value: " ++ show (pin, val, val')

    Nothing -> do
      -- We're causing one net to change.  Save the change, and check
      -- for each component connected to this net if action is needed.
      modifyNetVals $ Map.insert netKey val
      nets <- askNets
      let Just net = Map.lookup netKey nets
          netComps = Set.map fst net
      sequence_ $ map (checkComponent netKey) $ toList netComps


-- A pin can only be in one net.  Allow for user error here, since it
-- is possible to represent this inconsistency in the netlist data
-- structure.
pinNet :: Pin -> M v Int
pinNet pin = do
  nets <- askNets
  case Map.toList $ Map.filter (elem pin) nets of
    [(k,v)] -> return $ k
    nets -> fail $ "net: pin in zero or multiple nets: " ++ show (pin,nets)


-- Check if ready and if so, propagate.
checkComponent netKey comp =  do
  inputWait <- getInputWait
  let Just wait = Map.lookup comp inputWait
  case Set.null wait of
    True ->
      -- Skip components that are not waiting for input.  These have
      -- either already been evaluated, or do not need evaluation to
      -- begin with.
      return ()
    False -> do
      -- Remove this net from the component's input wait set.
      let wait' = Set.delete netKey wait
      modifyInputWait $ Map.insert comp wait' 
      case Set.null wait' of
        False ->
          -- Still waiting..
          return ()
        True ->
          -- Propagate if done waiting.
          evalComponent comp        

-- Precondition: all inputs are ready for evaluation.  Evaluate and
-- propagate through the network.
evalComponent :: forall v. Show v => Component -> M v ()
evalComponent comp = do

  semantics <- askSem
  netvals <- getNetVals
  inDeps <- askInDeps
  
  let
    -- What to provide to the Fun?
    inDeps' :: Map NetKey (Set Port)
    inDeps' = fromJust $ Map.lookup comp inDeps
    
    -- Compute list of input values, tagged with the ports they
    -- should be applied to.
    values :: [Maybe (v, Set Port)]
    values = toList $ Map.mapWithKey evalNet inDeps'
    evalNet :: NetKey -> (Set Port) -> Maybe (v, Set Port)
    evalNet n ports = fmap (,ports) $ Map.lookup n netvals
      
    -- This pattern match succeeds because of the precondition: all
    -- inputs should have values before evalComponent is invoked.
    Just values' = sequence values

    -- Rearrange inputs and compute outputs.
    inputs :: Map Port v
    inputs = foldr addVal Map.empty values'
    addVal (v, portSet) m = Map.union m $ Map.fromSet (const v) portSet

    -- Evaluate
    (fun, _) = semantics comp
    outputs = fun inputs

    -- Keep track of which pins are asserted, and recurse.
    outputs' = Map.mapKeys (comp,) outputs

  modifyOutputs $ Map.union outputs'
  drivePins outputs'





printl :: Show s => [s] -> IO ()
printl = sequence_ . (map print)



test = print $ eval sem netlist ins where

  -- Input/output "connector" pins.
  in_pin  = ("conn","in")
  out_pin = ("conn","out")

  -- We'll drive the input connector pin.
  ins = Map.fromList [(in_pin, True)]

  -- A single functional 2-port component, mapping the value of port
  -- "1" to port "2", negating it.
  inverter i =
    case Map.lookup "1" i of
      (Just v) ->  
        Map.fromList [("2", not v)]
      Nothing ->
        Map.empty

  -- Component u1 is assigned the inverter semantics.
  sem "u1"   = (inverter,         Set.fromList ["1"])
  -- The connector doesn't have a behavior
  sem "conn" = (const Map.empty,  Set.fromList [])
    
  netlist = Set.fromList [n1,n2]
  n1 = Set.fromList [("u1","1"), in_pin]
  n2 = Set.fromList [("u1","2"), out_pin]

