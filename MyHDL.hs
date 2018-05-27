{-# LANGUAGE FlexibleContexts #-}

module MyHDL where
import SeqNet
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import qualified Data.Set as Set

-- Simple print to MyHDL

-- Switch context as needed
setContextFor (Delay _) = need "seq"
setContextFor _         = need "comb"
need context = do
  (n, have) <- get
  case have == context of
    True -> return ()
    False -> do
      let state = (n+1, context)
      put state
      render state
render (n, "seq") = do
  tell $ "\t@always_seq(CLK.posedge, reset=RST)\n"
  tell $ "\tdef blk" ++ show n ++ "():\n"
render (n, "comb") = do
  tell $ "\t@always_seq(CLK.posedge, reset=RST)\n"
  tell $ "\tdef blk" ++ show n ++ "():\n"

sig n = "s" ++ show n
stmt (_,Input) = return()  -- No statement, already driven
stmt (n,c) = do
  setContextFor c
  tell $ "\t\t" ++ sig n ++ ".next = "
  tell $ node c ++ "\n"

node (Connect n) = sig n
node (Delay n) = sig n
node c = show c

-- FIXME:
-- instantiate local signals

type MyHDL = WriterT String (State (Int, String))

comb :: [(Int, Driver)] -> MyHDL ()
comb ns = do
  sequence_ $ [ stmt n | n <- ns ]
    

gen :: ([Int], Bindings) -> String
gen (ports, nodes) = w where
    
  m = do
    tell "def module(CLK,RST,"
    tell $ intercalate "," $ map sig ports
    tell "):\n"
    comb nodes

  (((), w), _) = runState (runWriterT m) (0, "")

