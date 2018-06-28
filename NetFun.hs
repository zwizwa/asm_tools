-- Given component semantics, convert a netlist to a function.

-- This uses terminology that is common circuit design:
-- . a netlist is a set of nets
-- . a net is a set of pins
-- . a pin is identified by a component id and a port id

-- ( Note that this is different from "net" indicating network.  Also
-- a "netlist" isn't a List, it is a Set. )

-- When each component can be mapped to a behaviour/semantics, it is
-- possible to (partially) evaluate a network's pins and/or nets.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

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

type Semantics v = Component -> (Fun v, FunIn)
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
  PinVals v -> Either String (PinVals v, NetVals v)

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

shortNets shorts nl = foldr short nl $ Map.toList shorts where
  short :: (NetName, Set NetName) -> NetList -> NetList
  short (name, netNames) netlist = netlist' where
    netlist' = Map.insert name unionNet netlist0
    netlist0 = foldr Map.delete netlist netNames
    unionNet = foldr (Set.union . net) Set.empty netNames
    net = fromJust . (flip Map.lookup netlist)

-- Alternative interface.  Keep name of first net in list.
shortNets' :: [[NetName]] -> NetList -> NetList  
shortNets' shorts = shortNets $ Map.fromList $ map tx shorts where
  tx names@(name:_) = (name, Set.fromList names)


-- The evaluation algorithm is abit more involved.

-- Depth first refinement is used: evaluate when all inputs to a
-- component's semantics function are vailable, then propagate the
-- effect of the new values.

-- Take into account failure: our representation does not impose any
-- constraints on how function semantics can be assigned to prots, so
-- it is possible multiple drivers are present.


-- The Monad stack used during evaluation.

type EvalState v = (NetVals v, InputWait, PinVals v)
type EvalEnv v = (Semantics v, NetList, InDeps)

newtype M v t =
  M { runM ::
        ReaderT (EvalEnv v)
       (StateT (EvalState v)
       (Either String)) t
    }
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
eval semantics netlist ins = rv' where

  -- Compute the initial state and environment.
  (inDeps, inputWait) = evalInit semantics netlist
  env = (semantics, netlist, inDeps)
  initState = (Map.empty, inputWait, Map.empty)

  -- Recursively evaluate, starting at inputs
  rv' = fmap rv $ runStateT (runReaderT (runM $ drivePins ins) env) initState
  rv ((), (nvs, _, pvs)) = (pvs, nvs) 


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
    (_, inPorts) = semantics comp


  -- and b) InputWait, used to incrementally keep track of whether all
  -- of a Component's input nets have a value.  The latter is a
  -- reduction of a).
  inputWait :: Map Component (Set NetName)
  inputWait = Map.map Map.keysSet inDeps

  



drivePins :: Show v => PinVals v -> M v ()
drivePins i = sequence_ $ map drivePin $ Map.toList i

drivePin :: Show v => (Pin, v) -> M v ()
drivePin (pin, val) = do

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
      nets <- askNetList
      let Just net = Map.lookup netKey nets
          netComps = Set.map fst net
      sequence_ $ map (checkComponent netKey) $ toList netComps


-- A pin can only be in one net.  Allow for user error here, since it
-- is possible to represent this inconsistency in the netlist data
-- structure.
pinNet :: Pin -> M v NetName
pinNet pin = do
  nets <- askNetList
  case Map.toList $ Map.filter (elem pin) nets of
    [(k,v)] -> return $ k
    nets -> fail $ "pinNet: zero or multiple nets: " ++ show (pin, nets)


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
    inDeps' = fromJust $ Map.lookup comp inDeps
    
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
    (fun, _) = semantics comp
    outputs = fun inputs

    -- Keep track of which pins are asserted, and recurse.
    outputs' = Map.mapKeys (comp,) outputs

  modifyOutputs $ Map.union outputs'
  drivePins outputs'





-- TESTS

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
    
  netlist = Map.fromList [("n1",n1),("n2",n2)]
  n1 = Set.fromList [("u1","1"), in_pin]
  n2 = Set.fromList [("u1","2"), out_pin]

