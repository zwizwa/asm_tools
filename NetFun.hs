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
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE TupleSections #-}
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
-- If the output is empty, it is assumed the input is incomplete.
type Fun v = Map Port v -> Map Port v


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
type Semantics v = Component -> Fun v

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

type EvalState v = (Map Int v, Set Component)
type EvalEnv v = (Semantics v, Map Int Net)
nets :: M v (Map Int Net)
nets = do (_,nets') <- ask ; return nets'

modifyNetVals    f = modify (\(nv,cs) -> (f nv, cs))
modifyComponents f = modify (\(nv,cs) -> (nv, f cs))

getNetVals :: M v (Map Int v)
getNetVals = do (nv,_) <- get ; return nv

getComponents :: M v (Set Component)
getComponents = do (_,cs) <- get ; return cs

newtype M v t = M { runM :: ReaderT (EvalEnv v) (StateT (EvalState v) (Either String)) t } deriving
  (Functor, Applicative, Monad,
   MonadReader (EvalEnv v),
   MonadState (EvalState v))

-- Inner routine.

-- FIXME: type confusion about ins.  from the outside, we need to
-- drive pins, so the interpretation should reflect that.  this kind
-- of confusion is really typical, missing a layer of wrapping.  For
-- now eval' takes pins as input.

-- Basic algorithm:
-- . for each input pin, find the corresponding net
-- . if net already has a value, raise an error
-- . set net value



type Signals v = Map Pin v

eval' :: Show v => Semantics v -> Netlist -> Signals v -> Either String (Signals v)
eval' sem netlist i = o where
  -- Assign each net a unique id, to relate env and state
  nets = Map.fromList $ zip [0..] $ Set.toList netlist
  env = (sem, nets)
  initState = (Map.empty, Set.empty)
  o = case runStateT (runReaderT (runM $ meval' i) env) initState of
        Left msg -> Left msg
        Right ((), (state, _)) -> Right Map.empty

meval' :: Show v => Signals v -> M v ()
meval' i = m where
  m = do
    sequence_ $ map setPin $ Map.toList i
    return ()
    
  setPin (pin, val) = do
    n <- pinNet pin
    nv <- getNetVals
    -- Something is wired wrong if the net already has a value.
    case Map.lookup n nv of
      Just val' ->
        fail $ "pin already has value: " ++ show (pin, val, val')
      Nothing -> do
        modifyNetVals $ Map.insert n val
        -- State has changed, so time to check if anything connected
        -- to this net can now evaluate.  Try evaluation on all
        -- components not yet marked as evaluated.
        done <- getComponents
        nets' <- nets 
        let net = Set.toList $ fromJust $ Map.lookup n nets'
            notDone c = not $ elem c done
            comps = filter notDone $ map fst $ net
        sequence_ $ map evalComp comps
            
  evalComp comp = do
    nets' <- nets
    let pins :: [Pin]
        pins = filter isComp $ append $ toList nets'
        isComp (c,_) = c == comp

    -- FIXME: Make this work first, then reduce algorithmic complexity
    -- if it is too slow.  It's possible to do this incrementally,
    -- e.g. by collecting all components in a todolist together with
    -- the inputs they need, to make it easier to determine if they
    -- can be evaluated.  It seems wasteful to recollect all inputs
    -- for each check.

    -- TODO:
    -- . Collect all defined pins connected to this component
    -- . Attempt evaluation
    -- . Abort if no success
    -- . Mark component as evaluated
    -- . Recurse on output pins
    return ()


-- A pin can only be in one net.  FIXME: Implementation detail: maybe
-- create a pin->net Map to avoid lookups?
pinNet :: Pin -> M v Int
pinNet pin = do
  nets' <- nets
  case Map.toList $ Map.filter (elem pin) nets' of
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
  sem "u1" = inverter
    
  netlist = Set.fromList [n1,n2]
  n1 = Set.fromList [("u1","1"), in_pin]
  n2 = Set.fromList [("u1","2"), out_pin]

