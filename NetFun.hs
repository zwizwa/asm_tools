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
-- specification.
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
-- particular Fun.  In general we would like to have a global
-- configuration that can be decoded into a configuration for each
-- NPort to associate a Fun on a component by component basis.
type Semantics = NPort -> Fun

-- The evaluation of a network can only be done relative to semantics.
-- But it is important to note that given a semantics, we can
-- "flatten" everything into a single function through signal
-- propagagion.  This is the main function we're interested in.
eval :: Semantics -> Net -> Fun
eval = undefined

-- In general this will need a way to perform proper "variable
-- renaming" for port identifiers.  This is why Port is implemented as
-- a list.
rename :: (NPort, Port) -> Port
rename (np,p) = np:p


-- Some temporary stubs for base types.  The String types are
-- identifiers that can be used for value retrieval in certain
-- contexts.
type Port = [String]
type NPort = String
type NPortType = String

-- Only support two-valued logic.
type Value = Bool

-- Configuration can be added later.  The idea is to create a family
-- of representations.
type FunFamily = Config -> Fun
type Config = ()   
