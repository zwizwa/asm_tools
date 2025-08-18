-- Context: Writing hand-optimized DSP routines for CortexA3 32-bit mode.
--
-- Development history
--
-- 1. I wrote a software pipelined loop against a timing measurment
--    routine running on the actual hardware.  While crafting the code
--    is straightforward in general, getting the details correct was
--    not.  Hard to test and debug raw machine code.
--
-- 2. I had gotten back into Haskell DSL work for another project so
--    thought I'd try again to write an emulator based on previous
--    work for the TI PRU microcontroller.  See
--    asm_tools/asm-tools-pru/Language/Pru (concrete, no tagless
--    final)
--
-- 3. Ended up with a tagless final DSL with assembly code generation
--    output and an emulator parameterized in the number data type,
--    which eventually allowed for:
--
--    a. Quickcheck tests using exact Integer number type (instead of
--       Float used on the real hardware).
--
--    b. Abstract interpretation on symbolic data, which allowed the
--       algorithm to produce output formula instead of output numbers
--       together with execution traces in terms of formula.  This
--       gave a much more direct idea about what was going wrong.
--
-- I really like this approach and would like to keep building on it.
-- Unfortunately I can not release the client's code that was crafted
-- and tested with this module, but I might be able to add some
-- examples later.  If you are interested, contact me.  I love talking
-- about these things.


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-} -- For type level functions
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}

module Language.Neon where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Fix
import qualified Data.List as List
import Data.List.Split
import qualified Data.Set as Set
import Data.IntMap.Strict
import Data.Dynamic
import GHC.TypeNats
import Control.Lens
import Control.Lens.TH
import Prelude hiding (lookup, log)
import Text.Printf

-- For indexing registers, scalar and vector memories and representing
-- native machine integers (32 bit ARM mode).
type N = Int

-- Register types
data D  = D N deriving Show -- Vector register
data R  = R N deriving Show -- Regular register

-- Literal
data L  = L N deriving Show

class MonadFix m => ASM m r | m -> r, r -> m where
  -- Instructions
  vld1_32  :: r D -> r R -> m ()
  vst1_32  :: r D -> r R -> m ()
  add      :: r R -> r R -> r L -> m ()
  subs     :: r R -> r R -> r L -> m ()
  sub      :: r R -> r R -> r L -> m ()
  nop      :: m ()
  vfma_f32 :: r D -> r D -> r D -> m ()
  ldr      :: r R -> r R -> r L -> m ()
 
  -- Operands
  l        :: N -> r L     -- Instruction literal
  r        :: N -> r R     -- ARM 32bit scalar register
  d        :: N -> r D     -- Neon 2 x 32bit vector register

  -- Control flow and addresses
  -- FIXME: Use a separate type than L because asm needs symbolic labels.
  
  label    :: m (r L)      -- Current code address
  b        :: (r L) -> m ()
  beq      :: (r L) -> m ()
  bne      :: (r L) -> m ()

  -- Debug
  comment :: String -> m ()
  comment _ = return ()


-- Compiler

-- State monad is used for label generation.
data CompState = CompState {
  _nextLabel :: Int
}

-- MonadFix is needed for forward declaration of labels.

-- Output is a MonadWriter that collects a list of instructions in ADT
-- form.  This Ins type is easy to write a Show instance to produce
-- the machine code.  Best to keep those two steps separated.

newtype CompM t = CompM (WriterT [Ins] (State CompState) t)
                deriving (Functor, Applicative, Monad,
                          MonadFix,
                          MonadWriter [Ins],
                          MonadState CompState)

data Rands = DP  D R
           | DDD D D D
           | RRL R R L
           | RL  R L
           | RPO R R L
           | Lbl L       -- FIXME: Should not be a literal
           | Void
           deriving Show

type Rater = String

data Ins = Ins Rater Rands
         | Label N
         deriving Show

data Comp t = Comp t


-- Lenses are overkill here.  Mostly to use the same approach as other
-- code in this project.
$(makeLenses ''CompState)



w :: Ins -> CompM ()
w ins = tell [ins]

-- Convert the tagless final DSL to Ins syntax list.
instance ASM CompM Comp where

  -- Instructions
  vld1_32  (Comp d)  (Comp r)            = w $ Ins "vld1.32"  $ DP d r
  vst1_32  (Comp d)  (Comp r)            = w $ Ins "vst1.32"  $ DP d r
  vfma_f32 (Comp d1) (Comp d2) (Comp d3) = w $ Ins "vfma.f32" $ DDD d1 d2 d3
  add      (Comp r1) (Comp r2) (Comp l)  = w $ Ins "add"      $ RRL r1 r2 l
  sub      (Comp r1) (Comp r2) (Comp l)  = w $ Ins "sub"      $ RRL r1 r2 l
  subs     (Comp r1) (Comp r2) (Comp l)  = w $ Ins "subs"     $ RRL r1 r2 l
  ldr      (Comp r)  (Comp p) (Comp o)   = w $ Ins "ldr"      $ RPO r p o
  nop                                    = w $ Ins "nop"      $ Void

  -- Operands
  l  = Comp . L
  r  = Comp . R
  d  = Comp . D

  -- Control flow
  bne      (Comp l)                      = w $ Ins "bne"      $ Lbl l
  beq      (Comp l)                      = w $ Ins "beq"      $ Lbl l
  b        (Comp l)                      = w $ Ins "b"        $ Lbl l

  label = do
    label' <- use nextLabel ; nextLabel += 1
    w $ Label label'
    return $ Comp $ L label'

  
-- Show instance to convert the Ins abstract syntax to concrete
-- assembly syntax.
data Asm = Asm [Ins]

showIns :: Ins -> String
showIns (Label l) = "l" ++ show l ++ ":\n"
 
showIns (Ins rater rands) = "\t" ++ rater ++ " " ++ (s rands) ++ "\n" where
  -- Show operands
  s :: Rands -> String
  s (DP   d r)      = comma $ [(sd d), (sp r)]
  s (DDD  d1 d2 d3) = comma $ fmap sd [d1, d2, d3]
  s (RRL  r1 r2 l)  = comma $ [(sr r1), (sr r2), (sl l)]
  s (RL   r l)      = comma $ [(sr r), (sl l)]
  s (RPO  r p o)    = comma $ [(sr r), (sp' p o)]
  s (Lbl  (L l))    = "l" ++ show l
  s Void            = ""
  --s other           = error $ "Rands: " ++ show other

  comma :: [String] -> String
  comma = mconcat . List.intersperse ", "

  -- Show d-reg, r-reg, literal, r-reg pointer dereference
  sd (D n) = "d" ++ show n
  sr (R n) = "r" ++ show n
  sl (L n) = "#" ++ show n
  sp r = "[" ++ sr r ++ "]"
  sp' r o = "[" ++ sr r ++ ", " ++ sl o ++ "]"

instance Show Asm where
  show (Asm ins) = mconcat $ fmap showIns ins

asm :: CompM () -> Asm
asm (CompM m) = Asm w where
  s0 = CompState 0
  ((_rv, w), _s) = runState (runWriterT m) s0



-- Evaluator

-- The evaluator is parameterized by the number type that represents
-- the vector data.  This allows:
-- . Float to emulate actual behavior
-- . Integer to allow for exact computation for QuickCheck tests
-- . Probe for algebraic evaluation, good for debugging

type V2 n = (n, n)
type RFile = IntMap Int
type DFile n = IntMap (V2 n)

-- Note that vector and integer memory are modeled as separate address
-- spaces.  There is no memory aliasing in the algorithm so that
-- should be fine, and it keeps the representation at a more
-- convenient higher level.

type MemV n = IntMap (V2 n)    -- Memory typed as V2 data
type MemI = IntMap Int   -- Memory typed as Int for pointers and counts

-- Code is represented as a graph of abstract instructions.  This
-- graph is Int-indexed wich is similar enough to the concrete target
-- machine.

type Code n = IntMap (EvalM n ())

type LogMsg = Char
data EvalState n = EvalState {
  _rfile :: RFile,
  _dfile :: DFile n,
  _memV  :: MemV n,
  _memI  :: MemI,
  _code  :: Code n,
  _ip    :: Int,
  _zFlag :: Bool
  }

-- Instructions can be executed directly or compiled into the Code
-- graph.  The latter is the default behavior.  The former is for
-- testing purposes.

data EvalMode = DirectMode | CompileMode
data EvalEnv = EvalEnv {
  _evalMode :: EvalMode
  }

newtype EvalM n t = EvalM (WriterT [LogMsg]         -- Execution traces
                          (ReaderT EvalEnv
                          (State (EvalState n))) t)  -- Machine emulation state
                deriving (Functor, Applicative, Monad, MonadFix,
                          MonadReader EvalEnv,
                          MonadWriter [LogMsg],
                          MonadState (EvalState n))

-- The monad produces a trace, the final machine state, and a generic
-- monadic result.
newtype EvalResult n t = EvalResult ([LogMsg], EvalState n, t)

data Eval n t = Eval t


-- Use lenses for the EvalState components.
$(makeLenses ''EvalState)
$(makeLenses ''EvalEnv)

log :: [ String ] -> EvalM nt ()
log = tell . concat

doRead
  :: String
     -> Getting (IntMap t) (EvalState nt) (IntMap t)
     -> t
     -> Key
     -> EvalM nt t

doRead tag lens dflt key = do
  map <- use lens
  case lookup key map of
    Nothing -> do
      -- Best to make these into warning messages and provide a default
      -- to keep the code total.
      log ["undefined ",tag," ",show key ,"\n"]
      return dflt
    Just val ->
      return val

readMemV :: Num n => Int -> EvalM n (V2 n)
readD    :: Num n => Int -> EvalM n (V2 n)

readMemI = doRead "scalar memory"   memI  0 
readMemV = doRead "vector memory"   memV  (0,0) 
readR    = doRead "scalar register" rfile 0
readD    = doRead "vector register" dfile (0,0)

doWrite
  :: ASetter (EvalState n) (EvalState n) (IntMap t) (IntMap t)
     -> Key -> t -> EvalM n ()
     
doWrite lens key val = do
  lens %= insert key val

writeMemV = doWrite memV
writeMemI = doWrite memI
writeD    = doWrite dfile
writeR    = doWrite rfile


-- Each instruction is wrapped with 'ins' to select the (default)
-- CompileMode behavior where the instruction is compiled into a graph
-- for later execution with 'run', or DirectMode behavior where the
-- instruction is executed directly against the machine state.  In
-- this mode the branch instructions do not work, as they only modify
-- the instruction pointer used by run.

ins :: forall n. (Show n, Num n) => EvalM n () -> EvalM n ()
ins mcode = do
  mode' <- asks (^. evalMode)
  case mode' of
    DirectMode  -> mcode
    CompileMode -> compile mcode

compile mcode = do      
  -- Append it to the IntMap
  (Eval (L addr)) <- label
  code %= insert addr mcode

run :: EvalM n ()
run = run' (-1)

-- Provide a limited run function for debugging infinite loops.
run' :: Integer -> EvalM n ()

run' 0 = do
  log [ "WARNING: count run count exceeded\n" ]
run' count = do
  ip' <- use ip
  -- If no branch instruction modifies the instruction pointer, we
  -- will execute the next instruction in line.  The mocde for branch
  -- instructions can modify ip if branch is taken.
  ip += 1
  code' <- use code
  case lookup ip' code' of
    Just mcode -> do
      -- Needs to be strict to avoid blowup.
      !() <- mcode
      run' $ count - 1
    Nothing ->
      -- The 0 address is reserved for halt.  Or any code that "falls
      -- off the end" of the code map.
      return ()

s :: Show t => t -> String
s = show 

-- Instruction logging produces columns for e.g. instruction and
-- annotation.  Combined with symbolic evaluation (Probe) this is a
-- very useful debugging tool.

logi :: [[ String ]] -> EvalM nt ()
logi cols = do
  ip' <- use ip
  let
      addr = printf "%3d " $ ip' - 1
      cols' = fmap ((printf "%-20s") . c) cols
      c = concat
  tell $ c $ [addr] ++ cols' ++ ["\n"]


instance forall n. (Show n, Num n) => ASM (EvalM n) (Eval n) where

  
  -- Instructions
  vld1_32 (Eval (D d)) (Eval (R r)) = ins $ do
    addr <- readR r
    val  <- readMemV addr
    writeD d val
    logi [["vld1_32 d",s d," r",s r],
          ["[",s addr,"] -> ", s val]]

  vst1_32 (Eval (D d)) (Eval (R r)) = ins $ do
    addr <- readR r
    val  <- readD d
    writeMemV addr val
    logi [["vst1_32 d",s d," r",s r],
          ["[",s addr, "] <- ", s val]]

  vfma_f32 (Eval (D acc)) (Eval (D x)) (Eval (D y)) = ins $ do
    (acc1, acc2) <- readD acc
    (x1,   x2)   <- readD x
    (y1,   y2)   <- readD y
    let acc' = (acc1 + x1 * y1,
                acc2 + x2 * y2)
    writeD acc acc'
    logi [["vfma_f31 d",s acc," d",s x," d",s y],
          [s acc']]
      
  add (Eval (R r1)) (Eval (R r2)) (Eval (L l)) = ins $ do
    v2 <- readR r2
    let v1 = v2 + l
    writeR r1 v1
    logi [["add r",s r1," r",s r2," ",s l],
          [s v1," = ",s v2," + ",s l ]]

  subs (Eval (R r1)) (Eval (R r2)) (Eval (L l)) = ins $ do
    v2 <- readR r2
    let v1 = v2 - l
    writeR r1 v1
    let zFlag' = (v1 == 0)
    zFlag .= zFlag'
    logi [["subs r",s r1," r",s r2," ",s l],
          [s v1," = ",s v2," - ",s l, ", z = ",s zFlag']]

  sub (Eval (R r1)) (Eval (R r2)) (Eval (L l)) = ins $ do
    v2 <- readR r2
    let v1 = v2 - l
    writeR r1 v1
    logi [["sub r",s r1," r",s r2," ",s l],
          [s v1," = ",s v2," - ",s l]]
    
  ldr (Eval (R r)) (Eval (R p)) (Eval (L offset)) = ins $ do
    addr <- readR p
    let addr' =  addr + offset
    val  <- readMemI addr'
    writeR r val
    logi [["ldr r",s r," r",s p," ",s offset],
          ["[",s addr'," = ",s addr, " + ", s offset,"] -> ", s val]]
    
  nop = return ()

  -- Operands
  l = Eval . L
  r = Eval . R
  d = Eval . D

  -- Control flow
  label = do
    -- The 1 + is to reserve address 0 to be not defined, this makes b
    -- 0 a halt instruction.
    map <- use code
    return $ Eval $ L $ 1 + size map

  b (Eval (L addr)) = ins $ do
    logi [[ "b ",s addr]]
    log ["\n"]
    ip .= addr

  beq (Eval (L addr)) = ins $ do
    zFlag' <- use zFlag
    logi [[ "beq ",s addr],
          ["z=",s zFlag']]
    when zFlag' $ do
      ip .= addr
      log ["\n"]

  bne (Eval (L addr)) = ins $ do
    zFlag' <- use zFlag
    logi [[ "bne ",s addr],
          ["z=",s zFlag']]
    when (not zFlag') $ do
      ip .= addr
      log ["\n"]

  -- Print to the trace each time execution passes here.
  comment str = ins $ do
    tell $ ";; " ++ str ++ "\n"


-- Format memory and register files.
showFile :: Show t => String -> IntMap t -> String
showFile tag map = concat $ lines where
  lines = fmap showCell $ toAscList map
  showCell (k, v) = tag ++ show k ++ " " ++ show v ++ "\n"
 
-- Show the entire machine state + monadic result.
instance (Show t, Show nt) => Show (EvalResult nt t) where
  show (EvalResult
         (log,
          EvalState rfile dfile memV memI code _ip _z,
          rv)) = str where
    str = "trace:\n" ++ log ++
          "rfile:\n" ++ showFile "r" rfile ++
          "dfile:\n" ++ showFile "d" dfile ++
          "memV:\n"  ++ showFile "" memV ++
          "memI:\n"  ++ showFile "" memI ++
          "code:\n" ++ (show $ size code) ++ " ins\n" ++
          "result:\n" ++ show rv ++ "\n"

im0 :: IntMap t
im0 = empty

eval :: EvalM n t -> EvalResult n t
eval (EvalM m) = EvalResult (l, s, rv) where
  s0 = EvalState im0 im0 im0 im0 im0 0 False
  e0 = EvalEnv CompileMode
  ((rv, l), s) = runState (runReaderT (runWriterT m) e0) s0


-- These are used for fixed size lists of registers.
-- FIXME: Move this elsewhere.

-- I want:
-- 1. type checking of register list size to parameterize assembly programs
-- 2. Functor, Foldable, Traversable to do abstraction over lists of registers

newtype Vec (n :: Nat) t = Vec [t] deriving (Functor, Foldable, Traversable)
unVec (Vec l) = l

vec4 :: (t,t,t,t)     -> Vec 4 t ; vec4 (a,b,c,d)     = Vec [a,b,c,d]
vec6 :: (t,t,t,t,t,t) -> Vec 6 t ; vec6 (a,b,c,d,e,f) = Vec [a,b,c,d,e,f]





-- Abstract interpretation for Eval,EvalM

-- This allows filling the memory with Var / Int and have program
-- output display the computation that the assembly language
-- performed.

data Probe i = Var String
             | Arr String i
             | Int Int
             | Add (Probe i) (Probe i)
             | Sub (Probe i) (Probe i)
             | Abs (Probe i)
             | Signum (Probe i)
             | Mul (Probe i) (Probe i)
             deriving (Eq)

probeOp op args = c $ ["(",op, c $ c $ fmap a args,")"] where
  c = concat
  a x = [" ",show x]

instance Show i => Show (Probe i) where

  show (Var s)   = s
  show (Arr s i) = s ++ show i
  
  show (Int i)    = show i
  show (Add a b)  = probeOp "+" [a,b]
  show (Sub a b)  = probeOp "-" [a,b]
  show (Mul a b)  = probeOp "*" [a,b]
  show (Abs a)    = probeOp "abs" [a]
  show (Signum a) = probeOp "signum" [a]
                 
instance Num (Probe i) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = Int . fromInteger

