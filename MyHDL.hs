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


gen :: (Eq n, Show n) => [Op n] -> [(n, Expr n)] -> String
gen ports bindings = str where
  m = mGen ports bindings
  (((), mode), str) =
    runReader (runWriterT (runStateT (runPrintMyHDL m) (0, None))) 0

mGen :: (Eq n, Show n) => [Op n] -> [(n, Expr n)] -> PrintMyHDL ()
mGen ports bindings = do
  let portNodes = map unNode ports
      unNode (Node n) = n
      unNode (Const n) = error $ "mGen: Const in ports: " ++ show ports
      internalNodes = filter isInternal $ map fst bindings
      isInternal n = not $ elem n portNodes
      name = "module"

  tell $ "def " ++ name ++ "("
  tell $ intercalate "," $ ["CLK","RST"] ++ map sig portNodes
  tell "):\n"

  indent (+ 1) $ do
    sequence_ $ map defSignal internalNodes
    indent (+ 1 ) $ sequence_ $ [assignment n e | (n, e) <- bindings]
    (n,_) <- get
    tab
    tell "return ["
    tell $ intercalate "," $ map blk [1..n]
    tell "]\n"
    -- tell $ "# " ++ show (nodeRefcounts nodes) ++ "\n"

blk n = "blk" ++ show n
sig n = "s" ++ show n

defSignal n = do
  tab ; tell $ sig n ++ " = Signal()\n"
  

assignment :: Show n => n -> (Expr n) -> PrintMyHDL ()
assignment n e@(Free (Compose (Delay _))) = do
  need Seq
  assignment' n e

assignment n e@(Free (Compose Input)) = do
  indent (+ (- 1)) $ do
    tab
    tell $ "# " ++ sig n ++ " is an input\n"
  
assignment n e = do
  need Comb
  assignment' n e

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
  
mExp (Pure n) = tell $ sig n
mExp (Free (Compose e)) = mTerm e

mTerm (Delay a)        = mOp a
mTerm (Connect a)      = mOp a
mTerm (Comb1 o a)      = call (show o) [mOp a]
mTerm (Comb2 o a b)    = infx o (mOp a) (mOp b)
mTerm (Comb3 IF c x y) = do  mOp x ; tell " if " ; mOp c ; tell " else " ; mOp y
mTerm Input            = call "INPUT" [] -- not reached

mOp (Const v) = tell $ show v
mOp (Node n)  = mExp n

call tag ms = do
  tell tag
  tell "("
  sequence_ $ intercalate [tell ","] $ map (\m->[m]) ms
  tell ")"

tab :: PrintMyHDL ()
tab = do
  n <- ask
  sequence_ $ [tell "\t" | _ <- [1..n]]
indent = local

infx o a b = do
  a ; tell " " ; tell (f2 o) ; tell " " ; b

f2 ADD = "+"
f2 SLL = "<<"
f2 SLR = ">>"
f2 XOR = "^"
f2 AND = "&"

