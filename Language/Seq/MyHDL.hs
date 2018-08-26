-- Simple print to MyHDL
-- See also the S-expression printer in SeqExpr.hs

-- MyHDL will reify the Python syntax to generating Verilog/VHDL.

-- The task here is to generate that syntax, and defer as much as
-- possible to function calls.  E.g. signal instantiation.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.Seq.MyHDL(myhdl,MyHDL,testbench,fpgaGen,fpgaWrite,noOutputCheck) where
import Language.Seq
import Language.Seq.Lib
import Data.AsmTools.CSV
import qualified Laguage.Seq.Expr as SeqExpr
import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Emu as SeqEmu
import Language.Seq.Term(Op(..),Term(..),NodeNum,Bindings)
import Language.Seq.Expr(Expr,Term')
import Language.Seq.PCF
import qualified Language.Seq.TestTools as TestTools

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Free
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Functor.Compose
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe
import Data.Bits hiding (bit)

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

type PortSpec = (String,Int)
data MyHDL = MyHDL [PortSpec] String
instance Show MyHDL where
  show (MyHDL specs code) = str where
    str = code ++ "\nports = " ++ show specs  ++ "\n"

-- Convert the port specification to a more specific type.
-- Note that reset value isn't necessary for I/O ports.
portSpec (name, (SInt (Just bits) rst)) = (name,bits)
-- portSpec (name, _) = (name,-1) -- FIXME
portSpec spec = error $ "portSpec needs bit size: " ++ show spec

-- Like Show, but generate a valid Python name from abstract node rep.
-- Also put the other constraints here.
class (Eq n, Show n) => Node n where
  nodeName :: n -> String

instance Node NodeNum where
  nodeName n = "s" ++ show n

instance Node String where
  nodeName n = n
  
myhdl :: Node n => String -> [Op n] -> [(n, Expr n)] -> String
myhdl name ports bindings = str where
  m = mGen name ports bindings
  (((), mode), str) =
    runReader (runWriterT (runStateT (runPrintMyHDL m) (0, None))) 0

mGen :: Node n => String -> [Op n] -> [(n, Expr n)] -> PrintMyHDL ()
mGen name ports bindings = do
  let portNodes = map unNode ports
      unNode (Node _ n) = n
      unNode (MemNode n) = n
      unNode (Const n) = error $ "mGen: Const in ports: " ++ show ports
      internalBindings = filter isInternal $ bindings
      isInternal (n,_) = not $ elem n portNodes

  tell $ "from myhdl import *\n"
  tell $ "from lib import *\n"
  tell $ "def " ++ name ++ "("
  tell $ commas $ ["CLK","RST"] ++ map sig portNodes
  tell "):\n"

  indent (+ 1) $ do
    sequence_ $ map defSignal internalBindings
    indent (+ 1 ) $ sequence_ $ [assignment n e | (n, e) <- bindings]

    ice40_reset
    
    (n,_) <- get
    tab
    tell "return ["
    tell $ commas $ map inst [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

-- FIXME: hardcoded hack for the ice40 reset generator.
ice40_reset = do
  i <- new_inst
  tab ; tell $ (inst i) ++ " = ice40_reset(CLK, RST)\n"


inst n = "inst" ++ show n
sig n = nodeName n

commas = intercalate ", "

defSignal :: Node n => (n, Expr n) -> PrintMyHDL ()

-- There are a couple of things I don't understand about how the
-- reflection works in MyHDL, but here's what works for now:

-- . Signals need to be instantiated above or inside a function
--   describing a module, but not below.  You can't "return" a signal
--   from a deeper instance.

-- . Placing instance constructors in objects gave problems.  It
--   appears as if that breaks the link from the run-time object to
--   the module code necessary for reflection.  It works when imported
--   and called directly.

defSignal (n, (Free (Compose (MemWr (_, o_wa, o_wd, _))))) = do
  i <- new_inst

  -- Assumes read and write are same size.
  let ta = SeqTerm.opType o_wa
      td = SeqTerm.opType o_wd
      prefix nm = sig n ++ "_" ++ nm
      assgn nm t = do
        tab ; tell $ nm ++ " = " ++ (sigSpec t) ++ "\n"
      mem_signals@[rd,we,wa,wd,ra] = map prefix ["rd","we","wa","wd","ra"]
      ram_signals = ["CLK",wa,wd,we,"CLK",ra,rd]
        
  sequence_ $ zipWith assgn mem_signals  [td, bit, ta, td, ta]
  tab ; tell $ (inst i) ++ " = ram(" ++ commas ram_signals ++ ")\n"

  

-- Other signals are defined.
defSignal (n, e) = do
  tab ; tell $ sig n ++ " = " ++ (sigSpec $ eType e) ++ "\n"

mask n v = v .&. ((1 `shiftL` n) - 1)

sigSpec (SInt Nothing _) = error "Signals need to be fully specified"
sigSpec (SInt (Just n) v0) = "Signal(modbv(" ++ (show $ mask n v0) ++ ")[" ++ show n ++ ":])"
eType :: Expr n -> SType
eType (Free (Compose t)) = SeqTerm.termType t
eType (Pure n) = error "eType doesn't work on node references"


assignment :: forall n. Node n => n -> (Expr n) -> PrintMyHDL ()
assignment n e@(Free (Compose (Delay _ _))) = do
  need Seq
  assign_next (sig n) (mExp e)
assignment n e@(Free (Compose (MemWr (we, wa, wd, ra)))) = do
  let src_sigs      = [mOp n | n  <- [we, wa, wd, ra]]
      mem_signames  = [sig n ++ "_" ++ s | s <- ["we","wa","wd","ra"] ]
  need Comb
  sequence_ $ zipWith assign_next mem_signames src_sigs
assignment n e@(Free (Compose (Input _))) = do
  indent (+ (- 1)) $ do
    tab ; tell $ "# input: " ++ sig n ++ "\n"
assignment n e = do
  need Comb
  assign_next (sig n) (mExp e)

assign_next :: String -> PrintMyHDL () -> PrintMyHDL ()
assign_next varName mExp' = do
  tab ; tell $ varName ++ ".next = " ; mExp' ; tell "\n"


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

-- FIXME: repurpose state to also track the memory instances.
new_inst :: PrintMyHDL Int
new_inst = do
  (n, context) <- get
  let state = (n+1, context)
  put state
  return $ n+1



render (n, Seq) = do
  tab ; tell $ "@always_seq(CLK.posedge, reset=RST)\n"
  tab ; tell $ "def inst" ++ show n ++ "():\n"
render (n, Comb) = do
  tab ; tell $ "@always_comb\n"
  tab ; tell $ "def inst" ++ show n ++ "():\n" 
render (_, None) =
  error $ "render: None"


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
mTerm (MemRd _ n@(MemNode _)) = do
  mOp n
  tell "_rd"

mTerm (MemRd _ _) =
  error $ "mTerm: MemRd: not a MemNode"
mTerm (MemWr _) =
  error $ "mTerm: MemWr: not reached"
  

showSize (Just s) = show s
showSize Nothing = ""

mOp :: Node n => Op (Free Term' n) -> PrintMyHDL ()

mOp (Const (SInt Nothing v))  = tell $ show v
mOp (Const (SInt (Just sz) v)) = tell $ show $ str where
  str' = showIntAtBase 2 intToDigit v "" 
  str  = replicate (sz - length str') '0' ++ str'

  

mOp (Node _ n) = mExp n
mOp (MemNode n) = mExp n

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

prfx _ [] =
  error $ "prfx: nullary not supported"
prfx op (o:os) = do
  tell $ op ++ "(" ; o
  sequence_ [tell ", " >> o | o <- os]
  tell ")"

infx o a b = do
  a ; tell " " ; tell (f2 o) ; tell " " ; b


f2 ADD = "+"
f2 MUL = "*"
f2 SUB = "-"
f2 SLL = "<<"
f2 SLR = ">>"
f2 OR  = "|"
f2 XOR = "^"
f2 AND = "&"
f2 EQU = "=="
f2 CONC = error $ "f2 CONC: handled elsewhere"






-- Don't write a Functor class for bindings.  If an ad-hoc functor
-- composition shows up, it is often easier to factor out just the
-- specialized mapping function.  Write the type first, then
-- implementation is straightforward.
mapBindings :: (a -> b) -> [(a, Term (Op a))] -> [(b, Term (Op b))]
mapBindings f l = map f' l where
  f' (name, term) = (f name, (fmap . fmap) f term)
  


type M = SeqTerm.M
type R = SeqTerm.R

-- Alternative interface used for generating FPGA images.
pyModule :: String -> [String] -> [SType] -> ([R S] -> M ()) -> MyHDL
pyModule name portNames portTypes mod = MyHDL portSpecs' pyCode where
  
  pyCode = myhdl name ports' $ SeqExpr.inlined bindings'

  -- Seq SeqTerm for some post processing steps that are shared
  -- between HDLs.
  (portSpecs', (ports', bindings')) =
    SeqTerm.hdl_compile portNames portTypes mod
 

-- For ice40 FPGA images, we use the convention that all ports are 1
-- bit wide.  This makes it easier to relate to the circuit netlist.
fpgaModule name (portNames, mod) = pyModule name portNames portTypes mod where
  portTypes = [Seq.SInt (Just 1) 0 | _ <- portNames]


-- Generate all files needed for MyHDL and Yosys to produce a binary
-- (.py module + .pcf pin map). This uses the convention that pin
-- names names are prefixed with underscores in the code, so capital
-- letters can be used in Haskell code.
fpgaGen name (names, fun) pinMap = (py, pcf') where 
  names' = map (\('_':nm) -> nm) names
  py = fpgaModule name (names', fun)
  pcf' = PCF ("CLK":"RST":names') pinMap

fpgaWrite name mod pins = do
  let (py,pcf) = fpgaGen name mod pins
  writeFile (name ++ ".py")  $ show py
  writeFile (name ++ ".pcf") $ show pcf









-- Test benches.
--
-- Note that when i/o busses are simplified to [r S], there are still
-- a couple of ways to represent code.
--
-- 1) m [r S]            passed into compileTerm , embedded Input nodes
-- 2) [r S] -> m ()      mirrors MyHDL port api, args can be in or out
-- 3) [r S] -> m [r S]   mirrors Seq processor, in and out explicit
--
-- MyHDL modules are our central idea, so we focus on the second
-- representation as the canonical one.  See pyModule above, which
-- performs the conversion from 2) to 1).

-- The code below performs conversion from 3) to 2), and generates
-- additional information to run the test bench, instantiating the
-- MyHDL module, applying inputs and collecting outputs.

-- Test benches are then constructed as:
-- . a MyHDL module
-- . a data structure to drive instantiation

-- For the test bench, the specification is derived from the SeqEmu
-- and SeqTH approaches: provide a [r S] -> m [r S] module, a list of
-- bit sizes, and an input list.

-- See run_myhdl.py

testbench ::
  String
  -> [Int]
  -- Rank 2, because we instantiate it twice.
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]]
  -> TestBench

data TestBench = TestBench MyHDL [[Int]] (Maybe [[Int]])
instance (Show TestBench) where
  show (TestBench hdl input output) = show hdl ++ input_py ++ output_py where
    input_py  = "\nins   = " ++ show input  ++ "\n"
    output_py = case output of
      Just output -> "\nouts  = " ++ show output ++ "\n"
      Nothing -> ""

noOutputCheck (TestBench hdl ins _) = TestBench hdl ins Nothing

testbench name inSizes mod input = tb where
  tb = TestBench hdl input $ Just $ output
  (portNames, portTypes, mod', output) = TestTools.testbench name inSizes mod input
  hdl = pyModule name portNames portTypes mod'

