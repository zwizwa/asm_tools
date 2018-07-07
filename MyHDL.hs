-- Simple print to MyHDL
-- See also the S-expression printer in SeqExpr.hs

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MyHDL(myhdl,MyHDL,testbench,fpga) where
import Seq
import qualified SeqExpr
import qualified SeqTerm
import qualified SeqEmu
import SeqTerm(Op(..),Term(..),NodeNum,Bindings)
import SeqExpr(Expr,Term')
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Functor.Compose


newtype PrintMyHDL t = PrintExpr {
  runPrintMyHDL :: StateT BlockState (WriterT String (Reader IndentLevel)) t
  }
  deriving (Functor, Applicative, Monad,
            MonadWriter String,
            MonadState BlockState,
            MonadReader IndentLevel)
type BlockState = (Int,Mode)
type IndentLevel = Int
data Mode = None | Seq | Comb deriving Eq

newtype MyHDL = MyHDL { unMyHDL :: String }
instance Show MyHDL where show = unMyHDL

-- Like Show, but generate a valid Python name from abstract node rep.
-- Also put the other constraints here.
class (Eq n, Show n) => Node n where
  nodeName :: n -> String

instance Node NodeNum where
  nodeName n = "s" ++ show n

instance Node String where
  nodeName n = n
  
myhdl :: Node n => [Op n] -> [(n, Expr n)] -> MyHDL
myhdl ports bindings = MyHDL str where
  m = mGen ports bindings
  (((), mode), str) =
    runReader (runWriterT (runStateT (runPrintMyHDL m) (0, None))) 0

mGen :: Node n => [Op n] -> [(n, Expr n)] -> PrintMyHDL ()
mGen ports bindings = do
  let portNodes = map unNode ports
      unNode (Node _ n) = n
      unNode (Const n) = error $ "mGen: Const in ports: " ++ show ports
      internalBindings = filter isInternal $ bindings
      isInternal (n,_) = not $ elem n portNodes
      name = "module"

  tell $ "from myhdl import *\n"
  tell $ "def " ++ name ++ "("
  tell $ intercalate ", " $ ["CLK","RST"] ++ map sig portNodes
  tell "):\n"

  indent (+ 1) $ do
    sequence_ $ map defSignal internalBindings
    indent (+ 1 ) $ sequence_ $ [assignment n e | (n, e) <- bindings]
    (n,_) <- get
    tab
    tell "return ["
    tell $ intercalate ", " $ map blk [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

blk n = "blk" ++ show n
sig n = nodeName n

defSignal (n, e) = do
  tab ; tell $ sig n ++ " = " ++ (sigSpec $ eType e) ++ "\n"

sigSpec (SInt Nothing _) = error "Signals need to be fully specified"
sigSpec (SInt (Just n) v0) = "Signal(modbv(" ++ show v0 ++ ")[" ++ show n ++ ":])"
eType :: Expr n -> SType
eType (Free (Compose t)) = SeqTerm.termType t
eType (Pure n) = error "eType doesn't work on node references"
  

assignment :: Node n => n -> (Expr n) -> PrintMyHDL ()
assignment n e@(Free (Compose (Delay _ _))) = do
  need Seq
  assignment' n e

assignment n e@(Free (Compose (Input _))) = do
  indent (+ (- 1)) $ do
    tab
    tell $ "# " ++ sig n ++ " is an input\n"
  
assignment n e = do
  need Comb
  assignment' n e

-- Mode printing.  Combinatorial expressions each get their own block
-- to provide proper sensitivity/signal semantics.  Sequential blocks
-- could all be joined together, but we keep them in source order.

need :: Mode -> PrintMyHDL ()
need context = do
  (n, have) <- get
  case (have == context, context) of
    -- Sequential blocks can be joined
    (True, Seq) ->
      return ()
    -- Comb->Comb or any change: create new block
    _ -> do
      let state = (n+1, context)
      put state
      indent (+ (-1)) $ render state
      
render (n, Seq) = do
  tab ; tell $ "@always_seq(CLK.posedge, reset=RST)\n"
  tab ; tell $ "def blk" ++ show n ++ "():\n"
render (n, Comb) = do
  tab ; tell $ "@always_comb\n"
  tab ; tell $ "def blk" ++ show n ++ "():\n" 

assignment' n e = do
  tab
  tell $ sig n ++ ".next = "
  mExp e
  tell "\n"

parens m = do -- (*)
  tell "(" ; m ; tell ")"
  
mExp (Pure n) = tell $ sig n
mExp (Free (Compose e)) = parens $ mTerm e -- (*)

-- (*) When to insert parenthesis?  Easiest is to do it for each
-- expression.  Python seems to allow this.


mTerm (Delay _ a)        = mOp a
mTerm (Connect _ a)      = mOp a
mTerm (Input _)          = call "INPUT" [] -- not reached, matched earlier
mTerm (Comb1 _ INV a)    = (tell "~") >> mOp a
mTerm (Comb2 _ CONC a b) = prfx "concat" $ map mOp [a,b]
mTerm (Comb2 _ o a b)    = infx o (mOp a) (mOp b)
mTerm (Comb3 _ IF c x y) = do
  mOp x
  tell " if "   ; mOp c
  tell " else " ; mOp y
mTerm (Slice _ a b c)    = do
  mOp a
  tell $ "[" ++ showSize b ++ ":"
  tell $ show c ++ "]"

showSize (Just s) = show s
showSize Nothing = ""

mOp (Const (SInt _ v))  = tell $ show v
mOp (Node _ n) = mExp n

call tag ms = do
  tell tag
  tell "("
  sequence_ $ intercalate [tell ", "] $ map (\m->[m]) ms
  tell ")"

tab :: PrintMyHDL ()
tab = do
  n <- ask
  sequence_ $ [tell "\t" | _ <- [1..n]]
indent = local

prfx op (o:os) = do
  tell $ op ++ "(" ; o
  sequence_ [tell ", " >> o | o <- os]
  tell ")"

infx o a b = do
  a ; tell " " ; tell (f2 o) ; tell " " ; b


f2 ADD = "+"
f2 SLL = "<<"
f2 SLR = ">>"
f2 XOR = "^"
f2 AND = "&"
f2 EQU = "=="



-- For run_testbench.py
-- FIXME: generalize this to I/O?
connectOut :: SeqTerm.M [SeqTerm.R S] -> SeqTerm.M [SeqTerm.R S]
connectOut mod = do
    out'   <- mod
    stypes <- sequence $ fmap stype out'
    out    <- SeqTerm.io stypes
    sequence_ $ zipWith connect out out'
    return out


-- Don't write a Functor class for bindings.  If an ad-hoc functor
-- composition shows up, it is often easier to factor out just the
-- specialized mapping function.  Write the type first, then
-- implementation is straightforward.

mapBindings :: (a -> b) -> [(a, Term (Op a))] -> [(b, Term (Op b))]
mapBindings f l = map f' l where
  f' (name, term) = (f name, (fmap . fmap) f term)


toPy :: SeqTerm.M [SeqTerm.R S] -> String
toPy mod = module_py where

  (ports, bindings) = SeqTerm.compile $ connectOut mod
  module_py = show $ myhdl ports' $ SeqExpr.inlined bindings'

  -- Replace nodes with named nodes.
  name :: NodeNum -> String
  name n = Map.findWithDefault ("s" ++ show n) n namedPorts
  namedPorts = Map.fromList $
    [(n, "p" ++ show i) | (Node _ n, i) <- zip ports [0..]]
  
  ports'    = (map . fmap) name ports
  bindings' = mapBindings  name bindings



-- See run_testbench.py
testbench :: Int -> (forall m r. Seq m r => m [r S]) -> (TestBench, [[Int]])
testbench n mod = (TestBench module_py output_py, output) where

  -- Emulation
  output = SeqEmu.trace mod
  output_py = "\noutput = " ++ show (take n $ output) ++ "\n"

  -- Code gen
  module_py = toPy mod

  
data TestBench = TestBench String String
instance (Show TestBench) where
  show (TestBench m o) = m ++ o
  


-- Alternative interface used for generating FPGA images.
fpga :: ([String], [SeqTerm.R S] -> SeqTerm.M ()) -> MyHDL
fpga (portNames, mod) = MyHDL module_py where

  -- Assume all input are single pins.
  pinType = (SInt (Just 1) 0)
  mod' = do
    io <- SeqTerm.io $ [pinType | _ <- portNames]
    mod io ; return io
    
  (ports, bindings) = SeqTerm.compile mod'
  module_py = show $ myhdl ports' $ SeqExpr.inlined bindings'

  -- Assign names
  ports'    = (map . fmap) name ports
  bindings' = mapBindings  name bindings
  name :: NodeNum -> String
  name n = Map.findWithDefault ("s" ++ show n) n namedPorts
  namedPorts = Map.fromList $
    [(n, nm) | (Node _ n, ('_':nm)) <- zip ports portNames]
