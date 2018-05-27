{-# LANGUAGE FlexibleContexts #-}

module MyHDL where
import SeqNet
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Free
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- Simple print to MyHDL

data Mode = None | Seq | Comb deriving Eq

-- Switch context as needed
setContextFor (Delay _) = need Seq
setContextFor _         = need Comb
need context = do
  (n, have) <- get
  case have == context of
    True -> return ()
    False -> do
      let state = (n+1, context)
      put state
      render state
render (n, Seq) = do
  tell $ "\t@always_seq(CLK.posedge, reset=RST)\n"
  tell $ "\tdef blk" ++ show n ++ "():\n"
render (n, Comb) = do
  tell $ "\t@always_comb\n"
  tell $ "\tdef blk" ++ show n ++ "():\n"

sig n = "s" ++ show n
stmt (n,Input) = do
  tell $ "\t" ++ "# input: " ++ sig n ++ "\n"
stmt (n,c) = do
  setContextFor c
  tell $ "\t\t" ++ sig n ++ ".next = "
  tell $ node c ++ "\n"

node (Connect n) = sig n
node (Delay n) = sig n
node (Const n) = show n
node c = show c

-- FIXME:
-- instantiate local signals

type MyHDL = WriterT String (State (Int, Mode))

signal n = do
  tell $ "\t" ++ sig n ++ " = Signal()\n"

blk n = "blk" ++ show n

gen :: ([Int], Bindings) -> String
gen (ports, nodes) = w where
  internalNodes = filter isInternal $ map fst nodes
  isInternal n = not $ elem n ports
  name = "module"
  m = do
    tell $ "def " ++ name ++ "(CLK,RST,"
    tell $ intercalate "," $ map sig ports
    tell "):\n"
    sequence_ $ map signal internalNodes
    sequence_ $ [ stmt n | n <- nodes ]
    (n,_) <- get
    tell "\treturn ["
    tell $ intercalate "," $ map blk [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

  (((), w), _) = runState (runWriterT m) (0, None)

