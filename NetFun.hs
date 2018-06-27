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

type EvalState v = Map Int v
type EvalEnv v = (Semantics v, Map Int Net)
nets :: M v (Map Int Net)
nets = do (_,nets') <- ask ; return nets'

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
eval' :: Semantics v -> Netlist -> Signals v -> Either String (Signals v)
eval' sem netlist i = o where
  -- Assign each net a unique id.
  nets = Map.fromList $ zip [0..] $ toList netlist
  env = (sem, nets)
  o = case runStateT (runReaderT (runM meval') env) Map.empty of
        Left msg -> Left msg
        Right ((), state) -> Right Map.empty

meval' :: M v ()
meval' = return ()

-- A pin can only be in one net.
net :: Pin -> M v (Int, Net)
net pin = do
  nets' <- nets
  case Map.toList $ Map.filter (elem pin) nets' of
    [kv] -> return $ kv
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

