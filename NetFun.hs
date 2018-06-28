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
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
--{-# LANGUAGE ExistentialQuantification #-}


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

-- Function inputs.
type FunIn = Set Port


-- It seems that FunType is not needed.  The necessary information can
-- be encoded in FunImpl: if not all inputs are available, then output
-- map is empty.

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
eval :: Semantics v -> Netlist -> Fun v
eval = undefined

-- In general this will need a way to perform proper "variable
-- renaming" for port identifiers.  .
rename :: (Component, Port) -> Port
rename (np,p) = np ++ ":" ++ p


-- Some temporary stubs for base types.  The String types are
-- identifiers that can be used for value retrieval in certain
-- contexts.

-- Configuration can be added later.  The idea is to create a family
-- of representations.
type FunFamily v = Config -> Fun v
type Config = ()   


-- Implementatation.  Algorithm is recursive refinement: evaluate when
-- all function inputs are available, then propagate on function
-- outputs.

-- Take into account failure: our representation does not impose any
-- constraints on how function semantics can be assigned to prots, so
-- it is possible multiple drivers are present.


type EvalState v = (Map Int v, InputWait)
type EvalEnv v = (Semantics v, Map Int Net, InDeps)

-- Input dependencies of components.  Computed initially and saved in
-- the environment.  A copy of this is threaded, popping net ids when
-- they are defined.  When the set becomes empty, the component is
-- ready to evaluate.
type InDeps    = Map Component (Map Int (Set Port))
type InputWait = Map Component (Set Int)

-- FIXME: Map Int (Set Port) could be computed when needed.  Factor it
-- out first.


askNets   :: M v (Map Int Net)
askSem    :: M v (Semantics v)
askInDeps :: M v InDeps

askNets   = do (_,nets',_)   <- ask ; return nets'
askSem    = do (sem',_,_)    <- ask ; return sem'
askInDeps = do (_,_,indeps') <- ask ; return indeps'

modifyNetVals   f = modify (\(nv,cs) -> (f nv, cs))
modifyInputWait f = modify (\(nv,cs) -> (nv, f cs))

getNetVals :: M v (Map Int v)
getNetVals = do (nv,_) <- get ; return nv

getInputWait :: M v InputWait
getInputWait = do (_,cs) <- get ; return cs

newtype M v t = M { runM :: ReaderT (EvalEnv v) (StateT (EvalState v) (Either String)) t } deriving
  (Functor, Applicative, Monad,
   MonadReader (EvalEnv v),
   MonadState (EvalState v))

-- Inner routine.

-- FIXME: type confusion about ins.  from the outside, we need to
-- drive pins, so the interpretation should reflect that.  this kind
-- of confusion is really typical, missing a layer of wrapping.  For
-- now eval' takes pins as input.

-- FIXME: this turned out to be really complicated.

-- Basic algorithm:
-- . for each input pin, find the corresponding net
-- . if net already has a value, raise an error
-- . set net value



type Signals v = Map Pin v

eval' :: Show v => Semantics v -> Netlist -> Signals v -> Either String (Signals v)
eval' semantics netlist ins = outs where

  -- Compute the initial state and environment.
  (allNets, indeps) = evalInit semantics netlist
  inputWait = Map.map Map.keysSet indeps

  -- Recursively evaluate, starting at inputs
  env = (semantics, allNets, indeps)
  initState = (Map.empty, inputWait)
  outs = case runStateT (runReaderT (runM $ meval' ins) env) initState of
           Left msg -> Left msg
           Right ((), _s) -> Right Map.empty

-- This is complex enough to merit factoring out.
evalInit semantics netlist = (allNets, indeps) where
  
  -- Assign each net a unique Int id, to relate env and state
  allNets :: Map Int Net
  allNets = Map.fromList $ zip [0..] $ Set.toList netlist

  -- Annotate each component with its input nets, identified by the
  -- Int id, and annotated with the ports connected to that net.
  -- Note: the latter could be factored out.
  
  indeps :: Map Component (Map Int (Set Port))
  indeps = Map.fromSet indep allComps
  allComps = Set.map fst $ Set.flatten netlist 
  indep comp = inNets where
    (_, inPorts) = semantics comp

    inPin :: Pin -> Set Port
    inPin (c, p) = case (c == comp) && (Set.member p inPorts) of
      True  -> Set.singleton p
      False -> Set.empty

    inNet :: Net -> Set Port
    inNet = Set.flatten . Set.map inPin
      
    inNets :: Map Int (Set Port)
    inNets = Map.filter (not . Set.null) $ Map.map inNet allNets


meval' :: forall v. Show v => Signals v -> M v ()
meval' i = m where
  m = do
    sequence_ $ map setPin $ Map.toList i
    return ()
    
setPin (pin, val) = do

  n <- pinNet pin
  nv <- getNetVals
  
  case Map.lookup n nv of
    
    Just val' ->
      -- Something is wired wrong if the net already has a value.
      fail $ "pin already has value: " ++ show (pin, val, val')

    Nothing -> do
      -- One net has changed:
      -- . Save change
      -- . Get a list of components on this net
      -- . Skip those that do not have a wait list
      -- . The others _must_ have this net in their wait list!!
      -- . Iterate over all the wait lists, removing this net
      -- . If any wait list is empty, remove wait list and propagate component

      modifyNetVals $ Map.insert n val
      nets <- askNets
      
      let checkComp comp = do
            inputWait <- getInputWait
            case Map.lookup comp inputWait of
              Nothing ->
                return ()
              Just inputSet -> do
                let inputSet' = Set.delete n inputSet
                case Set.null inputSet' of
                  False -> do
                    modifyInputWait $ Map.insert comp inputSet'
                    return ()
                  True -> do
                    modifyInputWait $ Map.delete comp
                    evalComp comp
                    
          Just net = Map.lookup n nets
          netComps = Set.map fst net
                    
      sequence_ $ map checkComp $ toList netComps
        

-- Precondition: all inputs are ready for evaluation.
-- Propagating component:
-- . Retreive all inputs
-- . Apply function
-- . Iterate over all outputs

-- This requires quite a bit of data structure shuffling.  Is there a
-- way to simplify that?

evalComp :: forall v. Show v => Component -> M v ()
evalComp comp = do

  semantics <- askSem
  indeps <- askInDeps
  netvals <- getNetVals
  
  let inNetInfo = fromJust $ Map.lookup comp indeps
      input :: Maybe (Map Int (v, Set Port))
      input = sequence $ Map.mapWithKey evalNet inNetInfo
      evalNet n portSet = fmap (,portSet) $ Map.lookup n netvals
      
  case input of
    Nothing ->
      fail $ "evalComp: inconsistency: inputs not ready: " ++ show comp
    Just input' -> do
      let ins :: Map Port v
          ins = foldr Map.union Map.empty $ map makeMap $ toList input' 
          makeMap (v, portSet) = Map.fromSet (const v) portSet
          (fun, _) = semantics comp
          outs = fun ins
      meval' $ Map.mapKeys (comp,) outs


-- A pin can only be in one net.  FIXME: Implementation detail: maybe
-- create a pin->net Map to avoid lookups?
pinNet :: Pin -> M v Int
pinNet pin = do
  nets <- askNets
  case Map.toList $ Map.filter (elem pin) nets of
    [(k,v)] -> return $ k
    nets -> fail $ "net: pin in zero or multiple nets: " ++ show (pin,nets)




printl :: Show s => [s] -> IO ()
printl = sequence_ . (map print)



test = print $ eval' sem netlist ins where

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
  sem "u1" = (inverter, Set.fromList ["1"])
    
  netlist = Set.fromList [n1,n2]
  n1 = Set.fromList [("u1","1"), in_pin]
  n2 = Set.fromList [("u1","2"), out_pin]

