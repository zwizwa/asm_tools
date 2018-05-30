-- Simple print to MyHDL
-- See also the S-expression printer in SeqExpr.hs

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module MyHDL(gen) where
import Seq
import qualified SeqExpr
import qualified SeqTerm
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

-- Pile some formatting machinery on top of the Free monad.
newtype PrintMyHDL t = PrintExpr {
  runPrintMyHDL :: StateT BlockState (WriterT String (ReaderT IndentLevel (Free Term'))) t
  }
  deriving (Functor, Applicative, Monad,
            MonadWriter String,
            MonadState BlockState,
            MonadReader IndentLevel)
type BlockState = (Int,Mode)
type IndentLevel = Int
data Mode = None | Seq | Comb deriving Eq


gen :: Show n => [Op n] -> [(n, Expr n)] -> String
gen ports bindings = str where
  m = mGen ports bindings
  Pure (((), mode), str) =
    runReaderT (runWriterT (runStateT (runPrintMyHDL m) (0, None))) 0

mGen :: Show n => [Op n] -> [(n, Expr n)] -> PrintMyHDL ()
mGen ports bindings = do
  let internalNodes = [] -- filter isInternal $ map fst bindings
      -- isInternal n = not $ elem n ports
      name = "module"

  tell $ "def " ++ name ++ "(CLK,RST"
  -- tell $ intercalate "," $ map sig ports
  tell "):\n"

  indent (+ 1) $ do
    indent (+ 1 ) $ sequence_ $ [statement n e | (n, e) <- bindings]
    (n,_) <- get
    tab
    tell "return ["
    tell $ intercalate "," $ map blk [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

blk n = "blk" ++ show n

sig n = "s" ++ show n

statement :: Show n => n -> (Expr n) -> PrintMyHDL ()
statement n e@(Free (Compose (Delay _))) = do
  need Seq
  statement' n e
  
statement n e = do
  need Comb
  statement' n e

need :: Mode -> PrintMyHDL ()
need context = do
  (n, have) <- get
  case have == context of
    True -> return ()
    False -> do
      let state = (n+1, context)
      put state
      indent (+ (-1)) $ render state
render (n, Seq) = do
  tab
  tell $ "@always_seq(CLK.posedge, reset=RST)\n"
  tab
  tell $ "def blk" ++ show n ++ "():\n"
render (n, Comb) = do
  tab
  tell $ "@always_comb\n"
  tab
  tell $ "def blk" ++ show n ++ "():\n" 

statement' n e = do
  tab
  tell $ sig n ++ ".next = "
  mExp e
  tell "\n"
  
mExp (Pure n) = tagged "NODE" [tell $ show n]
mExp (Free (Compose e)) = mTerm e

mTerm Input           = tagged "INPUT"   []
mTerm (Delay a)       = tagged "DELAY"   [mOp a]
mTerm (Connect a)     = tagged "CONNECT" [mOp a]
mTerm (Comb1 o a)     = tagged (show o)  [mOp a]
mTerm (Comb2 o a b)   = tagged (show o)  [mOp a, mOp b]
mTerm (Comb3 o a b c) = tagged (show o)  [mOp a, mOp b, mOp c]

mOp (Const v) = tagged "CONST" [ tell $ show v]
mOp (Node n)  = mExp n

tagged tag ms = do
  tell "{"
  tell tag
  sequence_ $ map ((tell " ") >>) ms
  tell "}"

tab :: PrintMyHDL ()
tab = do
  n <- ask
  sequence_ $ [tell "\t" | _ <- [1..n]]
indent = local


-- data Mode = None | Seq | Comb deriving Eq

-- -- Switch context as needed
-- setContextFor (Free (Node (Delay _))) = need Seq
-- setContextFor _                       = need Comb
-- need context = do
--   (n, have) <- get
--   case have == context of
--     True -> return ()
--     False -> do
--       let state = (n+1, context)
--       put state
--       render state
-- render (n, Seq) = do
--   tell $ "\t@always_seq(CLK.posedge, reset=RST)\n"
--   tell $ "\tdef blk" ++ show n ++ "():\n"
-- render (n, Comb) = do
--   tell $ "\t@always_comb\n"
--   tell $ "\tdef blk" ++ show n ++ "():\n" 

-- sig n = "s" ++ show n

-- -- Unpack the levels:

-- assignment :: (NodeNum, Expr (Op NodeNum)) -> M ()
-- assignment (n, Free (Node Input)) = do
--   tell $ "\t# " ++ sig n ++ " is an input\n"
-- assignment (n, c) = do
--   setContextFor c
--   tell $ "\t\t" ++ sig n ++ ".next = "
--   tell $ expr c ++ "\n"

-- expr (Pure (Node  n)) = sig n
-- expr (Free (Const c)) = show c
-- expr (Free (Node  e)) = op e

-- op (Comb2 f a b)   = f2 f a b
-- op (Comb3 f a b c) = f3 f a b c
-- op (Delay a) = expr a -- Delay is implemented by seq context
-- op (Connect e) = expr e
-- op e = show e

-- ifx f a b = expr a ++ " " ++ f ++ " " ++ expr b
  
-- f2 ADD = ifx "+"
-- f2 SLL = ifx "<<"
-- f2 SLR = ifx ">>"
-- f2 XOR = ifx "^"
-- f2 AND = ifx "&"
-- -- f2 op = \a b -> show (op,a,b)

-- -- FIXME: if is different.  does MyHDL support ternery if?  Seems so..  Check this.
-- -- f3 IF c a b = "if " ++ expr c ++ " then " ++ expr a ++ " else " ++ expr b
-- f3 IF c a b = expr a ++ " if " ++ expr c ++ " else " ++ expr b



-- -- FIXME:
-- -- instantiate local signals

-- -- It's private, so this is "the" monad.
-- type M = WriterT String (State (Int, Mode))

-- defSignal n = do
--   tell $ "\t" ++ sig n ++ " = Signal()\n"

-- blk n = "blk" ++ show n

-- gen :: ([NodeNum], Bindings NodeNum) -> String
-- gen (ports, terms) = w where
--   exprs = inlined terms
--   internalNodes = filter isInternal $ map fst exprs
--   isInternal n = not $ elem n ports
--   name = "module"
--   m = do
--     tell $ "def " ++ name ++ "(CLK,RST,"
--     tell $ intercalate "," $ map sig ports
--     tell "):\n"
--     sequence_ $ map defSignal internalNodes
--     sequence_ $ [ assignment e | e <- exprs ]
--     (n,_) <- get
--     tell "\treturn ["
--     tell $ intercalate "," $ map blk [1..n]
--     tell "]\n"
--     -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

--   (((), w), _) = runState (runWriterT m) (0, None)

