-- Given component semantics, convert a netlist to a function.

-- This uses terminology that is common circuit design:
-- . a netlist is a set of nets
-- . a net is a set of pins
-- . a pin is identified by a component id and a port id

-- ( Note that this is different from "net" indicating network.  Also
-- a "netlist" isn't a List, it is a Set. )

-- When each component can be mapped to a behaviour/semantics, it is
-- possible to (partially) evaluate a network's pins and/or nets.

-- TODO: A key insight that required some iteration is that:
-- . partitions are the natural form of netlists
-- . a strict order can produce representatives if needed
-- . representatives are useful for temporary associations


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module NetFun where

import Data.Set (Set(..))
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set
import Control.Monad.Reader
import Control.Applicative
import Data.Foldable
import Data.Either
import Data.Maybe
-- Laziness is not necessary, so use strict collections when possible.
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import qualified SetMap as SetMap
import qualified Partition as Partition

-- Basic collection hierarchy.  Note that nets need to be named to
-- allow for information to be "tagged onto" the net, such as logic
-- value after evaluation.
type NetList = Map NetName Net
type Net     = Set Pin
type Pin     = (Component, Port)

-- Identifiers are implemented as strings.  When appropriate, the
-- context contains dictionaries that can resolve these to other
-- types.
type Port      = String
type Component = String
type NetName   = String

-- It is not necessary to define the component list seprately, as it
-- can be derived directly from the graph:
components :: NetList -> Set Component
components = Set.map fst . Set.flatten . Set.fromList . toList





-- The evaluation of a network can be done based on Semantics.  This
-- needs a description of the inputs and an abstract function to
-- implement behavior.  A Fun is a map from input to output.

type Semantics v = Component -> (FunIn, Fun v)
type Fun v = Map Port v -> Map Port v
type FunIn = Set Port

-- Note that the I/O signature is not encoded at the Haskell type
-- level to allow for easily parameterizable semantics, e.g. some
-- ports change between input and output based on a parameter.



-- Given a set of pins that are externally driven, compute the pins
-- that will be asserted in response to this, together with the
-- (partial) evaluation state of the nets.

-- Either, because evaluation can fail if the NetList or Semantics are
-- inconsistent.  Part of the responsibility of this code is to
-- identify such inconsistencies in a design.

eval :: Show v =>
  Semantics v -> NetList ->
  PinVals v -> (PinVals v, NetVals v)

type PinVals v = Map Pin v
type NetVals v = Map NetName v



-- In a practical emulation, it is can be useful to split or join
-- components to be able to assign semantics at ta different
-- granularity than what is encoded in the concrete circuit netlest.

-- This manipulation is better performed as a (syntactic) netlist
-- transformation, keeping the network evaluation routine simple (One
-- behavior per Component).

type ComponentTransform = Component -> Port -> Maybe (Component, Port)
transformComponents :: ComponentTransform -> NetList -> NetList


-- Another transformation is to create shorts.  Shorts cannot be
-- modeled by the evaluator (relations vs. functions).  Therefore
-- implement them as a structural transformation.
type Shorts = Map NetName (Set NetName)
shortNets :: Shorts -> NetList -> NetList






-- IMPLEMENTATION

-- Component transformation and shorting are straightforward with the
-- proper representation, so let's get those out of the way first.

transformComponents ctx net = net' where
  net' = Map.map (Set.map f) net
  f pin@(comp,port) =
    case ctx comp port of
      Nothing   -> pin
      Just pin' -> pin'







-- An aliased netlist is just this:
type NetList' = Map (Set NetName) Net
aliasedNet :: NetList -> NetList'
aliasedNet = Map.mapKeys Set.singleton

-- How to perform lookup and merge?  Merge is easy, Take two nets,
-- remove them, and insert a new net which unions the names and the
-- contents.  Lookup involves set operations.

-- Can this be reduced to implementing Eq for a newtype-wrapped Set?
-- E.g. two sets are Eq if their intersection is not null.  This is
-- not transitive: 




-- mergeNets (Set NetName) -> (Set NetName) -> NetList' -> NetList'
-- mergeNets n1 n2 nl = nl' where
--   n
--   nl' = undefined



-- Lookup will need a way to promote a representative to
-- the key (set).






-- shortNets shorts netlist = undefined where
  -- This is based on a morphism between these two functions
  -- NetName -> Set NetName
  -- Net -> Set Net (which can be flattened to Net)


  -- One problem with just folding is that the names get lost.  One
  -- way to solve this is to represent netnames as sets, and then
  -- flatten them in a final step?
  
  

  

-- FIXME: I'm still not convinced this is entirely correct, since I
-- had to fix the "origNL" bug.  Why is this hard to express at all?

shortNets shorts origNL =  foldr short origNL $ Map.toList shorts where
  short :: (NetName, Set NetName) -> NetList -> NetList
  short (name, netNames) netlist = netlist' where

    -- Note that the names are only valid in the original netlist.
    net :: NetName -> Net
    net name   = fromJust' (err name) $ Map.lookup name origNL
    err name   = "shortNets: " ++ show name

    shortedNet :: Net
    shortedNet = foldr Set.union Set.empty $ Set.map net netNames

    netlist0  = foldr Map.delete netlist netNames
    netlist'  = Map.insert name shortedNet netlist0


-- Alternative interface.  Keep name of first net in list.
shortNets' :: [[NetName]] -> NetList -> NetList  
shortNets' shorts = shortNets $ Map.fromList $ map tx shorts where
  tx names@(name:_) = (name, Set.fromList names)


-- The evaluation algorithm is abit more involved.

-- Depth first refinement is used: evaluate when all inputs to a
-- component's semantics function are vailable, then propagate the
-- effect of the new values.

-- The Monad stack used during evaluation.

type EvalState v = (NetVals v, InputWait, PinVals v)
type EvalEnv v = (Semantics v, NetList, InDeps)

newtype M v t =
  M { runM :: ReaderT (EvalEnv v) (State (EvalState v)) t }
  deriving (Functor, Applicative, Monad,
            MonadReader (EvalEnv v),
            MonadState (EvalState v))


-- These extra data structures guide evaluation.

-- As net values gradually become available during evaluation, the Set
-- of inputs a component is waiting for will shrink.  When it becomes
-- empty, a component is ready to be evaluated.
type InputWait = Map Component (Set NetName)

-- Once all inputs are available, the (Map Port v) input to Fun can be
-- constructed.  The following index data structure is used in that
-- process.  Note the use of (Set Port) instead of Port: it is
-- possible that a Component has multiple ports connected to the same
-- net.
type InDeps = Map Component (Map NetName (Set Port))


-- Monadic state and environemnt access boilerplate.

askNetList :: M v NetList
askSem     :: M v (Semantics v)
askInDeps  :: M v InDeps

askNetList = do (_,nets',_)   <- ask ; return nets'
askSem     = do (sem',_,_)    <- ask ; return sem'
askInDeps  = do (_,_,indeps') <- ask ; return indeps'

modifyNetVals   f = modify (\(vs, ws, os) -> (f vs, ws, os))
modifyInputWait f = modify (\(vs, ws, os) -> (vs, f ws, os))
modifyOutputs   f = modify (\(vs, ws, os) -> (vs, ws, f os))

getNetVals :: M v (NetVals v)
getNetVals = do (nv,_,_) <- get ; return nv

getInputWait :: M v InputWait
getInputWait = do (_,cs,_) <- get ; return cs


-- Main evaluation entry point.  See above for type.
eval semantics netlist ins = (pvs, nvs) where

  -- Compute the initial state and environment.
  (inDeps, inputWait) = evalInit semantics netlist
  env = (semantics, netlist, inDeps)
  initState = (Map.empty, inputWait, Map.empty)

  -- Recursively evaluate, starting at inputs
  ((), (nvs, _, pvs)) =
    runState (runReaderT (runM $ drivePins ins) env) initState


-- Compute run-time data structures.
evalInit :: Show v => Semantics v -> NetList -> (InDeps, InputWait)
evalInit semantics netlist = (inDeps, inputWait) where
  
  -- The data structure that guides network recursion relates a
  -- Component to it's Fun's input dependencies.  There are two
  -- versions of this:

  -- a) InDeps, which has additional Port information used to prepare
  -- the Fun's input Map.  We bundle that information because it is
  -- readily available during the necessary traversal, avoiding a
  -- search during evaluation.

  inDeps :: Map Component (Map NetName (Set Port))
  inDeps = Map.fromSet inDep allComps
  allComps = components netlist 
  inDep comp = inNets where
    
    -- all of comp's pins
    inNets :: Map NetName (Set Port)
    inNets = Map.filter (not . Set.null) $ Map.map inNet netlist

    -- ... for a single network
    inNet :: Net -> Set Port
    inNet = Set.flatten . Set.map inPin

    -- ... for a single pin in a network
    -- isomophic to :: Pin -> Maybe Port, but easier to compose
    inPin :: Pin -> Set Port
    inPin (c, p) = case (c == comp) && (Set.member p inPorts) of
      True  -> Set.singleton p
      False -> Set.empty
    (inPorts, _) = semantics comp


  -- and b) InputWait, used to incrementally keep track of whether all
  -- of a Component's input nets have a value.  The latter is a
  -- reduction of a).
  inputWait :: Map Component (Set NetName)
  inputWait = Map.map Map.keysSet inDeps

  



drivePins :: Show v => PinVals v -> M v ()
drivePins i = sequence_ $ map drivePin $ Map.toList i

drivePin :: Show v => (Pin, v) -> M v ()
drivePin (pin, val) = do
  nets <- askNetList
  netVals <- getNetVals
  let Just netKey = pinNet nets pin
  
  case Map.lookup netKey netVals of
    
    Just val' ->
      -- Something is wired wrong if the net already has a value.
      error $ "pin already has value: " ++ show (pin, val, val')

    Nothing -> do
      -- We're causing one net to change.  Save the change, and check
      -- for each component connected to this net if action is needed.
      modifyNetVals $ Map.insert netKey val
      nets <- askNetList
      let Just net = Map.lookup netKey nets
          netComps = Set.map fst net
      sequence_ $ map (checkComponent netKey) $ toList netComps


-- A pin can only be in one net.  Allow for user error here, since it
-- is possible to represent this inconsistency in the netlist data
-- structure.

-- Not connected pins can happen when starting from components instead
-- of nets.  Pins in multiple nets are a bad inconsistency and should
-- just be an error.

pinNet :: NetList -> Pin -> Maybe NetName
pinNet nets pin =
  case Map.toList $ Map.filter (elem pin) nets of
    [] -> Nothing
    [(k,v)] -> Just k
    nets -> error $ "pinNet: multiple nets: " ++ show (pin, nets)


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
    inDeps' :: Map NetName (Set Port)
    inDeps' = fromJust' "inDeps" $ Map.lookup comp inDeps
    
    -- Compute list of input values, tagged with the ports they
    -- should be applied to.
    values :: [Maybe (v, Set Port)]
    values = toList $ Map.mapWithKey evalNet inDeps'
    evalNet :: NetName -> (Set Port) -> Maybe (v, Set Port)
    evalNet n ports = fmap (,ports) $ Map.lookup n netvals
      
    -- This pattern match succeeds because of the precondition: all
    -- inputs should have values before evalComponent is invoked.
    Just values' = sequence values

    -- Rearrange inputs and compute outputs.
    inputs :: Map Port v
    inputs = foldr addVal Map.empty values'
    addVal (v, portSet) m = Map.union m $ Map.fromSet (const v) portSet

    -- Evaluate
    (_, fun) = semantics comp
    outputs = fun inputs

    -- Keep track of which pins are asserted, and recurse.
    outputs' = Map.mapKeys (comp,) outputs

  modifyOutputs $ Map.union outputs'
  drivePins outputs'


-- Single case matchers with some failure message tagging.
fromJust' e (Just v) = v
fromJust' e Nothing = error $ e ++ ": fromJust' failed"



-- TESTS

test = print $ eval sem netlist ins where

  s = Set.fromList
  m = Map.fromList

  -- Input/output "connector" pins.
  in_pin  = ("conn","in")
  out_pin = ("conn","out")

  -- We'll drive the input connector pin.
  ins = m[(in_pin, True)]

  -- A single functional 2-port component, mapping the value of port
  -- "1" to port "2", negating it.
  inverter i =
    case Map.lookup "1" i of
      (Just v) ->  
        m[("2", not v)]
      Nothing ->
        m[]

  -- Component u1 is assigned the inverter semantics.
  -- The connector doesn't have a behavior
  sem "u1"   = (s["1"], inverter)
  sem "conn" = (s[],    const $ m[])
    
  netlist = m[("n1",n1),("n2",n2)]
  n1 = s[("u1","1"), in_pin]
  n2 = s[("u1","2"), out_pin]

