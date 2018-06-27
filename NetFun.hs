-- Convert a network (graph) to a function.

-- A network consists of:
-- . a set of n-port instances, each with a type
-- . a set of n-port types, implementing I/O functions (parameterized)
-- . a set of nets, mapping equating (instance,port)

-- Sets are represented as lists.

-- It is possible to convert a network to a function as long as
-- n-ports are functions.


module NetFun where

import Data.Map.Lazy (Map(..))
import Data.Set (Set(..))
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

-- A Fun is a map from input to output, together with a type
-- specification.  Both are parameterized by a configuration.
-- E.g. some ports can change from input to output.

type FunFamily = Config -> Fun
type Fun = (FunType, FunImpl)
type FunImpl  = Map Port Value -> Map Port Value
type FunType = (Set Port, Set Port)


-- A Net is a set of Node.  A Node is a set of NPort Ports.  For each
-- NPort there is a type function.
type Net = (NPort -> NPortType, Set Node)
type Node = (NPort, Port)

-- It is not necessary to define the component list seprately, as it
-- can be derived directly from the graph:
nports :: Net -> Set NPort
nports (_, nodes) = Set.map fst nodes

-- A semantics _instance_ assigns each NPort (not NPortType!) to a
-- particular Fun.
interpret :: NPort -> Fun
interpret = undefined

-- If we have a map from global configuration to configuration for
-- each function, we can construct a parameterized representation.

-- The evaluation of a network can only be done relative to semantics.
-- But it is important to note that given a semantics, we can
-- "flatten" everything into a single function.
eval :: (NPort -> Fun) -> Net -> Fun
eval = undefined

-- In general this will need a way to perform proper "variable
-- renaming" for port identifiers.  This is why Port is implemented as
-- a list.
rename :: (NPort, Port) -> Port
rename (np,p) = np:p


-- Some temporary stubs for base types.
type Config = ()   
type Port = [String]
type Value = Bool
type NPort = String
type NPortType = String

