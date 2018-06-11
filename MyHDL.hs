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
sig n = "s" ++ show n

defSignal (n, e) = do
  tab ; tell $ sig n ++ " = " ++ (sigSpec $ eType e) ++ "\n"

sigSpec (SInt Nothing _) = error "Signals need to be fully specified"
sigSpec (SInt (Just n) v0) = "Signal(modbv(" ++ show v0 ++ ")[" ++ show n ++ ":])"
eType :: Expr n -> SType
eType (Free (Compose t)) = SeqTerm.termType t
eType (Pure n) = error "eType doesn't work on node references"
  

assignment :: Show n => n -> (Expr n) -> PrintMyHDL ()
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

mTerm (Delay _ a)        = mOp a
mTerm (Connect _ a)      = mOp a
mTerm (Comb1 _ o a)      = call (show o) [mOp a]
mTerm (Comb2 _ CONC a b) = prfx "concat" $ map mOp [a,b]
mTerm (Comb2 _ o a b)    = infx o (mOp a) (mOp b)
mTerm (Comb3 _ IF c x y) = do mOp x ; tell " if " ; mOp c ; tell " else " ; mOp y
mTerm (Slice _ a b c)    = do mOp a ; tell $ "[" ++ showSize b ++ ":" ++ show c ++ "]"
mTerm (Input _)          = call "INPUT" [] -- not reached

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

