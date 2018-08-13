-- Simple print to MyHDL
-- See also the S-expression printer in SeqExpr.hs

-- MyHDL will reify the Python syntax to generating Verilog/VHDL.

-- The task here is to generate that syntax, and defer as much as
-- possible to function calls.  E.g. signal instantiation.



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
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module MyHDL(myhdl,MyHDL,testbench,fpga,fpga',PCF(..),pcf,run) where
import Seq
import SeqLib
import CSV
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
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import System.Process
import System.IO

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
  
myhdl :: Node n => String -> [Op n] -> [(n, Expr n)] -> MyHDL
myhdl name ports bindings = MyHDL str where
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
  tell $ "def " ++ name ++ "("
  tell $ commas $ ["env","CLK","RST"] ++ map sig portNodes
  tell "):\n"

  indent (+ 1) $ do
    sequence_ $ map defSignal internalBindings
    indent (+ 1 ) $ sequence_ $ [assignment n e | (n, e) <- bindings]
    (n,_) <- get
    tab
    tell "return ["
    tell $ commas $ map blk [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

blk n = "blk" ++ show n
sig n = nodeName n

commas = intercalate ", "

defSignal :: Node n => (n, Expr n) -> PrintMyHDL ()

-- Memories are special.  Instantiation will be done in the framework.
-- Here we just unpacl the ports.
defSignal (n, (Free (Compose (MemWr _)))) = do
  let mem_prefix = nodeName n
      mem_signals =
        [ mem_prefix ++ "_" ++ sig
        | sig <- ["rd","we","wa","wd","ra"]]
      mem_signals' = "[" ++ commas mem_signals ++ "]"
  tab ; tell $ "# " ++ mem_prefix ++ " is a memory\n"
  tab ; tell $ mem_signals' ++ " = env.memory(\"" ++ mem_prefix ++ "\")\n"

-- Other signals are defined.
defSignal (n, e) = do
  tab ; tell $ sig n ++ " = " ++ (sigSpec $ eType e) ++ "\n"

sigSpec (SInt Nothing _) = error "Signals need to be fully specified"
sigSpec (SInt (Just n) v0) = "env.sig(" ++ commas [show n, show v0] ++ ")"
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
    tab
    tell $ "# " ++ sig n ++ " is an input\n"
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
      
render (n, Seq) = do
  tab ; tell $ "@always_seq(CLK.posedge, reset=RST)\n"
  tab ; tell $ "def blk" ++ show n ++ "():\n"
render (n, Comb) = do
  tab ; tell $ "@always_comb\n"
  tab ; tell $ "def blk" ++ show n ++ "():\n" 
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



-- For run_myhdl.py
-- FIXME: generalize this to I/O?
type M = SeqTerm.M
type R = SeqTerm.R
connectIO :: [SType] -> ([R S] -> M [R S]) -> M [R S]
connectIO inTyp mod = do
  -- FIXME: assumes inputs are binary
  in'    <- SeqTerm.inputs inTyp
  out'   <- mod in'
  stypes <- sequence $ fmap stype out'
  out    <- SeqTerm.inputs stypes  -- When assigned, type changes from in->out
  sequence_ $ zipWith connect out out'
  return $ in' ++ out

toPy :: String -> [SType] -> ([R S] -> M [R S]) -> String
toPy name inTyp mod = module_py where

  (ports, bindings) = SeqTerm.compileTerm $ connectIO inTyp mod
  module_py = show $ myhdl name ports' $ SeqExpr.inlined bindings'

  -- Replace nodes with named nodes.
  rename :: NodeNum -> String
  rename n = Map.findWithDefault ("s" ++ show n) n namedPorts

  namedPorts :: Map.Map NodeNum String
  namedPorts = Map.fromList $
    [(n, "p" ++ show i) | (Node _ n, i) <- zip ports [0..]]
  
  ports'    = (map . fmap) rename ports
  bindings' = mapBindings  rename bindings




data TestBench = TestBench String String String
instance (Show TestBench) where
  show (TestBench m i o) = m ++ i ++ o

testbench ::
  String
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]]
  -> (TestBench, [[Int]])
testbench name mod input = (TestBench module_py input_py output_py, output) where

  -- See run_myhdl.py
  n = length input
  nb_in = length $ head input

  -- Types. Stick with just bits
  inTyp = [bits 1 | _ <- [1..nb_in]]
  inBitSizes = [s | SInt (Just s) _ <- inTyp]
  
  -- Emulation
  output = SeqEmu.iticks (SeqEmu.onInts inBitSizes mod) input
  output_py = "\nouts = " ++ show (take n $ output) ++ "\n"
  input_py  = "\nins  = " ++ show input ++ "\n"

  -- Code gen
  module_py = toPy name inTyp mod


-- Don't write a Functor class for bindings.  If an ad-hoc functor
-- composition shows up, it is often easier to factor out just the
-- specialized mapping function.  Write the type first, then
-- implementation is straightforward.
mapBindings :: (a -> b) -> [(a, Term (Op a))] -> [(b, Term (Op b))]
mapBindings f l = map f' l where
  f' (name, term) = (f name, (fmap . fmap) f term)
  


-- Alternative interface used for generating FPGA images.
fpga :: String -> ([String], [R S] -> M ()) -> MyHDL
fpga name (portNames, mod) = MyHDL module_py where

  -- Assume all input are single pins.
  pinType = (SInt (Just 1) 0)
  mod' = do
    -- When assigned, type changes from in->out
    io <- SeqTerm.inputs $ [pinType | _ <- portNames]
    mod io ; return io
    
  (ports, bindings) = SeqTerm.compileTerm mod'
  module_py = show $ myhdl name ports' $ SeqExpr.inlined bindings'

  -- Assign names
  ports'    = (map . fmap) rename ports
  bindings' = mapBindings  rename bindings
  rename :: NodeNum -> String
  rename n = Map.findWithDefault ("s" ++ show n) n namedPorts
  namedPorts = Map.fromList $
    [(n, nm) | (Node _ n, nm) <- zip ports portNames]

-- Using the convention that names are prefixed, so capital letters
-- can be used in Haskell code.  Also generates pcf based on pin map.
fpga' name (names, fun) pinMap = (py, pcf') where 
  names' = map (\('_':nm) -> nm) names
  py = fpga name (names', fun)
  pcf' = PCF ("CLK":"RST":names') pinMap

--- ice40 PCF pin configuration files
pcf :: [String] -> (String -> String) -> String
pcf names pin = pcf' where
  pcf' = concat $ map set_io names
  set_io name =
    "set_io " ++
    name ++ " " ++
    (pin name) ++ "\n"

data PCF = PCF [String] (String->String)
instance Show PCF where
  show (PCF names pin) = pcf names pin



-- Running MyHDL
run :: String -> IO ()
run funName = do
  let cmd = "PYTHONPATH=myhdl python3 run_myhdl.py " ++ funName ++ " " ++ funName ++ ".py"
  putStrLn "cmd:"
  putStrLn cmd
  (_, Just stdout, Just stderr, _) <-
    createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
  out <- hGetContents' stdout
  err <- hGetContents' stderr

  putStrLn "stdout:"
  putStr out
  putStrLn "stderr:"
  putStr err
  
  -- hClose stdout   -- FIXME: how to use lazy IO
  return ()

-- Workaround for lazy IO
hGetContents' h = do
  str <- hGetContents h
  seq str (return str)
