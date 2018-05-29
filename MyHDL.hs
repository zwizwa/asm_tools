{-# LANGUAGE FlexibleContexts #-}

module MyHDL where
import Seq
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
setContextFor (Free (Node (Delay _))) = need Seq
setContextFor _                       = need Comb
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

-- Unpack the levels:
-- a) assignment statments
assignment (n, Free (Node Input)) = do
  tell $ "\t# " ++ sig n ++ " is an input\n"
assignment (n,c) = do
  setContextFor c
  tell $ "\t\t" ++ sig n ++ ".next = "
  tell $ expr c ++ "\n"
-- b) expressions
expr (Pure (Node  n)) = sig n
expr (Free (Const ...
expr (Free (Node  e)) = op e
-- 
op (Const c) = show c
op (Comb2 f a b)   = f2 f a b
op (Comb3 f a b c) = f3 f a b c
op (Delay a) = expr a -- Delay is implemented by seq context
op (Connect e) = expr e
op e = show e

ifx f a b = expr a ++ " " ++ f ++ " " ++ expr b
  
f2 ADD = ifx "+"
f2 SLL = ifx "<<"
f2 SLR = ifx ">>"
f2 XOR = ifx "^"
f2 AND = ifx "&"
-- f2 op = \a b -> show (op,a,b)

-- FIXME: if is different.  does MyHDL support ternery if?  Seems so..  Check this.
-- f3 IF c a b = "if " ++ expr c ++ " then " ++ expr a ++ " else " ++ expr b
f3 IF c a b = expr a ++ " if " ++ expr c ++ " else " ++ expr b



-- FIXME:
-- instantiate local signals

type MyHDL = WriterT String (State (Int, Mode))

defSignal n = do
  tell $ "\t" ++ sig n ++ " = Signal()\n"

blk n = "blk" ++ show n

gen :: ([NodeNum], Bindings NodeNum) -> String
gen (ports, terms) = w where
  exprs = inlined terms
  internalNodes = filter isInternal $ map fst exprs
  isInternal n = not $ elem n ports
  name = "module"
  m = do
    tell $ "def " ++ name ++ "(CLK,RST,"
    tell $ intercalate "," $ map sig ports
    tell "):\n"
    sequence_ $ map defSignal internalNodes
    sequence_ $ [ assignment e | e <- exprs ]
    (n,_) <- get
    tell "\treturn ["
    tell $ intercalate "," $ map blk [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

  (((), w), _) = runState (runWriterT m) (0, None)

