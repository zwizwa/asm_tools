-- Network rendering of a Seq program 
-- Used as a base for MyHDL rendering.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module SeqTerm where
import Seq(SType(..),Env,initEnv)
import qualified Seq as Seq
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Data.List
import Data.Maybe
import Data.Map.Strict (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Compose
import Data.Functor.Classes

-- Seq does not provide a way to create modules with abstract I/O.
-- Specific constructs in this module are used to compose basic Seq
-- operations into something that can be rendered as a MyHDL module.

-- Seq is not a full HDL.  In a typical setting, a circuit will
-- contain pure combinatorial logic, and possibly different clock
-- domains.  Seq cannot simulate those, so some simulation needs to be
-- handed off to MyHDL.  This is the main reason to provide module
-- output: to be included in a larger discrete event simulation.

-- The basic idea is to:
-- 1) Use Seq to design sequential circuits
-- 2) Use MyHDL to perform simulation and HDL export
-- 3) Use Verilog/VHDL compiler to synthesize logic


data Term n
  = Comb1   SType Seq.Op1 n
  | Comb2   SType Seq.Op2 n n
  | Comb3   SType Seq.Op3 n n n
  | Slice   SType n Seq.SSize Seq.NbBits
  | Delay   SType n
  | MemRd   SType n
  | MemWr   (n,n,n,n)
  | Connect SType n
  | Input   SType -- Externally driven node
  deriving (Show, Functor, Foldable)

-- Constants are not monadic values in Seq (tried that, and decided
-- it's too annoying), so the operand type Op has two clauses:
data Op n
  = Node    SType n   -- node reference (*)
  | Const   SType     -- inlined constant
  | MemNode n
  deriving (Show, Functor, Foldable)

-- ???
-- Workaround: use sexpr formatting.  more readable anyway
--instance Show1 Term where
--  liftShowsPrec = ()


-- (*) Type-annotated because due to use of writer monad, there is no
-- access to the dictionary to retreive node type from a plain
-- reference.  This introduces copying and some places where there are
-- representable invalid states needing asserts, plus the awkward type
-- selectors here.  How to fix that?
termType (Comb1 t _ _) = t
termType (Comb2 t _ _ _) = t
termType (Comb3 t _ _ _ _) = t
termType (Slice t _ _ _) = t
termType (Delay t _) = t
termType (Connect t _) = t
termType (Input t) = t
termType (MemRd t _) = t
termType (MemWr _) =
  -- (SInt (Just 123) 123)
  error $ "termType: MemWr"

opType (Const t) = t
opType (Node t _) = t
opType (MemNode _) = error $ "opType: MemNode"


type NodeNum  = Int
type PortNum  = Int
type ConstVal = Int


-- It's convenient if the Drivers preserve order in the way they are
-- recorded to make sure definition dominates use.

newtype M t = M { unM ::
                    ReaderT (Env R)
                    (WriterT (Bindings NodeNum)
                     (State CompState))
                    t
                } deriving
  (Functor, Applicative, Monad,
   MonadReader (Env R),
   MonadWriter (Bindings NodeNum),
   MonadState CompState)

data Binding n = Binding n (Term (Op n)) |
                 Probe (Op n) String
type Bindings n = [Binding n]
type CompState = NodeNum

-- Primitive state manipulations
modifyNodeNum :: (NodeNum -> NodeNum) -> M ()
modifyNodeNum = modify
getNodeNum :: M NodeNum
getNodeNum = get


-- Phantom representation wrapper
data R t = R { unR :: Op NodeNum }  

instance Seq.Seq M R where

  constant t = R $ Const t

  -- undriven signal
  signal typ   = fmap R $ makeNode typ
  stype (R op) = return $ opType op

  -- driven nodes

  -- FIXME: use Seq.hs SType functions
  -- This does not perform the necessary checks.
  
  op1 o (R a) =
    fmap R $ driven $ Comb1 (combTypes [a]) o a

  op2 o (R a) (R b) = m where
    ta@(SInt sza _) = opType a
    tb@(SInt szb _) = opType b
    sz = fromRight' $ Seq.op2size o sza szb
    m = fmap R $ driven $ Comb2 (SInt sz 0) o a b

  op3 o (R a) (R b) (R c) = m where
    ta@(SInt sza _) = opType a
    tb@(SInt szb _) = opType b
    tc@(SInt szc _) = opType c
    sz = fromRight' $ Seq.op3size o sza szb szc
    m = fmap R $ driven $ Comb3 (SInt sz 0) o a b c

  slice (R a) upper lower = m where
    sz = Seq.slice2size upper lower
    m = fmap R $ driven $ Slice (SInt sz 0) a upper lower

  -- Combinatorial drive is needed to support combinatorial module
  -- outputs, but otherwise not used in Seq.hs code.
  connect = connect'
  -- register drive
  update  = delay'

  -- see comments in Seq.erl
  getEnv = ask
  withEnv = local

  memory td = do
    mem  <- fmap MemNode makeNodeNum
    rData <- driven $ MemRd td mem
    return (R rData, R mem)

  updateMemory (R (MemNode n)) (R wEn, R wAddr, R wData, R rAddr) = do
    driveNode n $ MemWr (wEn, wAddr, wData, rAddr)
  updateMemory _ _ = error $ "updateMemory: bad argument"

  probe name (R n) = tell $ [Probe n name]

fromRight' (Right a) = a
fromRight' (Left e) = error e


-- Note: these should use proper unification.  For now, this is a workaround.

-- For delay, the type constraint comes from the destination register.
delay' (R (Node t dst)) (R src) = do
  driveNode dst $ Delay t src
delay' _ _ = do
  error $ "delay': bad argument"

-- For connect, type is propageated from the source.  See MyHDL.hs
connect' (R (Node _ dst)) (R src) = do
  driveNode dst $ Connect (opType src) src
connect' _ _ = do
  error $ "connect': bad argument"



-- For constants.
instance Num (R Seq.S) where
  fromInteger i = R $ Const $ SInt Nothing $ fromInteger i
  -- Implement the rest just for constants.
  (+) = num2 (+)
  (*) = num2 (*)
  abs = num1 abs
  signum = num1 signum
  negate = num1 negate

num1 f (R (Const (SInt s a))) =
  R $ Const $ SInt s $ f a
num1 _ _ = error $ "num1: bad argument"
  
num2 f (R (Const (SInt sa a))) (R (Const (SInt sb b))) =
  R $ Const $ SInt (mergeSize [sa,sb]) $ f a b
num2 _ _ _ = error $ "num2: bad argument"


-- Note that this only works for combinatorial results, which do not
-- have an initial value, but do have a bit width.  Another indication
-- of data structures not being encoded well.
combTypes :: [Op NodeNum] -> SType
combTypes ns = SInt size 0 where
  sizes = [s | (SInt s _) <- map opType ns]
  size = mergeSize sizes

mergeSize :: [Maybe Seq.NbBits] -> Maybe Seq.NbBits
mergeSize [] = error "mergeSize internal error"
mergeSize [t] = t
mergeSize (ta:tb:ts) = mergeSize ((f ta tb):ts) where
  f sz Nothing = sz
  f Nothing sz = sz
  f (Just a) (Just b) = Just $ max a b


makeNode :: SType -> M (Op NodeNum)
makeNode t = fmap (Node t) makeNodeNum

makeNodeNum :: M NodeNum
makeNodeNum = do
  n <- getNodeNum
  modifyNodeNum (+ 1)
  return n
  

driven c = do
  s@(Node _ n) <- makeNode $ termType c
  driveNode n c
  return s


-- To support "connect", i/o ports start out as input, and will switch
-- role when driven.  Note that the Input nodes will appear in the
-- compiled output in order.
inputs :: [SType] -> M [R Seq.S]
inputs ts = sequence $ map input ts

input :: SType -> M (R Seq.S)
input t = fmap R $ driven $ Input t

-- ... and convert them to output here.  Other nodes cannot be driven
-- more than once.  Note: using fix, this error is avoided.
driveNode n c = do
  tell [Binding n c]
  
-- Compile to list of I/O ports and network map.
compileTerm :: M [R Seq.S] -> ([Op NodeNum], [(NodeNum, Term (Op NodeNum))])
compileTerm m = (ports, bindings) where
  (ports, bindings, _) = compileTerm' m

compileTerm' :: M [R Seq.S] -> ([Op NodeNum],
                                [(NodeNum, Term (Op NodeNum))],
                                [(Op NodeNum, String)])
compileTerm' m = (map unR ports, cleanPorts nodes, probes) where
  
  nodes  = catMaybes $ map (fst . node) bindings
  probes = catMaybes $ map (snd . node) bindings
  
  node (Binding n e)  = (Just (n, e), Nothing)
  node (Probe n s) = (Nothing, Just (n, s))

  ((ports, bindings), nbNodes) =
    runState (runWriterT (runReaderT (unM m) initEnv)) 0

  -- Remove duplicates, keeping last, preserving order.
  -- FIXME: generalize to functor output?
  cleanPorts ports = ports' where
    ports' = reverse $ f Set.empty $ reverse ports
    f _ [] = []
    f s (n@(p,_):ns) =
      case p `Set.member` s of
        True -> f s ns
        False -> (n : f (p `Set.insert` s) ns)


-- Compile an i/o function based on types.  Note that Input nodes will
-- appear in order in the bindings.

compileFun ::
  [SType]
  -> ([R Seq.S] -> M [R Seq.S])
  -> ([Op NodeNum], [(NodeNum, Term (Op NodeNum))])
compileFun ts fm = compileTerm $ (inputs ts) >>= fm




data Part = Delays | Inputs | MemRds | MemWrs | Exprs deriving Eq

-- Useful for postprocessing
partition bindings t = map snd $ filter ((t ==) . fst) tagged where
  tagged = map p' bindings
  p' x = (p x, x)
  p (_, Input _)   = Inputs
  p (_, Delay _ _) = Delays
  p (_, MemRd _ _) = MemRds
  p (_, MemWr _)   = MemWrs
  p _              = Exprs




-- S-expression formatting
mTerm sub (Input _)         = tagged "INPUT"   []
mTerm sub (Delay _ a)       = tagged "DELAY"   [mOp sub a]
mTerm sub (Connect _ a)     = tagged "CONNECT" [mOp sub a]
mTerm sub (Comb1 _ o a)     = tagged (show o)  [mOp sub a]
mTerm sub (Comb2 _ o a b)   = tagged (show o)  [mOp sub a, mOp sub b]
mTerm sub (Comb3 _ o a b c) = tagged (show o)  [mOp sub a, mOp sub b, mOp sub c]
mTerm sub (Slice _ a b c)   = tagged "SLICE"   [mOp sub a, tell $ showSize b, tell $ show c]
mTerm sub (MemRd _ a)       = tagged "MEMRD"   [mOp sub a]
mTerm sub (MemWr (a,b,c,d)) = tagged "MEMWR"   [mOp sub a, mOp sub b, mOp sub c, mOp sub d]

mOp _   (Const v)   = tagged "CONST" [ tell $ showType v ]
mOp sub (Node _ n)  = sub n
mOp sub (MemNode n) = sub n

showSize (Just s) = show s
showSize Nothing = "_"
                                            
showType (SInt sz v) = showSize sz ++ ":" ++ show v

tagged tag ms = do
  tell "("
  tell tag
  sequence_ $ map ((tell " ") >>) ms
  tell ")"



sexp' :: Show n => [(n, Term (Op n))] -> String
sexp' bindings =
  concat [concat [show n, " <- ", sexp e, "\n"] | (n, e) <- bindings]

-- Pile some formatting machinery on top of the Free monad.
newtype PrintTerm t = PrintTerm {
  runPrintTerm :: WriterT String (Reader IndentLevel) t
  }
  deriving (Functor, Applicative, Monad,
            MonadWriter String,
            MonadReader IndentLevel)
type IndentLevel = Int


sexp :: Show n => Term (Op n) -> String
sexp e = str where
  ((), str) = runReader (runWriterT (runPrintTerm $ mSexp e)) 0

mSexp :: Show n => Term (Op n) -> PrintTerm ()
mSexp e = mTerm sub e where
  sub n = tagged "NODE" [tell $ show n]


-- Render probe names unique so they can be used as global circuit names.
probeNames probes = probes' where
  -- Throw out constants
  nodeNum (Node _ n) = Just n
  nodeNum (MemNode n) = Just n
  nodeNum _ = Nothing
  
  probes' = [(node,name) | (name,node) <- Map.toList uniques]
  uniques = foldr unique Map.empty probes
  unique (node, name) map = map' where
    map' = case nodeNum node of
      Just n -> Map.insert name' n map
      Nothing -> map
    name' = case Map.lookup name map of
      Nothing -> name
      Just n -> case node of
        (Node _ n) -> name ++ show n
        _ -> error "probeNames: constant"
  
