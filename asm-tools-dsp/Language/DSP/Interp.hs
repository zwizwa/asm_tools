{-# LANGUAGE
ScopedTypeVariables,
GADTs
#-}

-- Attempt to solve the "monad hiding" problem by splitting the
-- problem into making an typed DSL as a GADT, combined with an
-- explicit interpreter.

module Language.DSP.Interp where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

-- Example from http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf

-- data Term t where
--   Zero   :: Term Int
--   Succ   :: Term Int -> Term Int
--   Pred   :: Term Int -> Term Int
--   IsZero :: Term Int -> Term Bool
--   If     :: Term Bool -> Term t -> Term t -> Term t
--   T      :: Term Bool
--   F      :: Term Bool
            
-- eval :: forall t. Term t -> t
-- eval Zero = 0
-- eval (Succ a) = eval a + 1
-- eval (Pred a) = eval a - 1
-- eval T = True
-- eval F = False
-- eval (IsZero a) = eval a == 0
-- eval (If c t f) = if eval c then eval t else eval f


-- To get started, create a very simple language and implement a
-- Haskell evaluator and a compiler to tagged syntax.

-- SYNTAX

-- FIXME: Replace Num class with a class that does Num behavior +
-- construct type tag.

class Prim t where
  addPrim :: t -> t -> t
  mulPrim :: t -> t -> t
  typePrim :: Term t -> PrimType

data PrimType = PFloat | PInt deriving Show

instance Prim Float where
  addPrim = (+)
  mulPrim = (*)
  typePrim _ = PFloat
  
instance Prim Int where
  addPrim = (+)
  mulPrim = (*)
  typePrim _ = PInt




data Opcode2 = Add | Mul deriving Show
data Term t where
  F    :: Float -> Term Float
  I    :: Int   -> Term Int
  Op2  :: Prim t => Opcode2 -> Term t -> Term t -> Term t
  Lam  :: (Term t -> Term t') -> Term (t -> t')
  App  :: Term (t -> t') -> Term t -> Term t'
  Sig  :: Prim t => Val t -> Term (t -> (t,  t')) -> Term t'
  Tup2 :: Term t -> Term t' -> Term (t,t')

  -- Some variants used only in implementation: variable introduction
  -- for compilation, and construction of language terms from lists
  -- for evaluation.
  VarRep  :: Int -> Term t
  ListRep :: [t] -> Term t

-- Constant term as used for Sig init
data Val t where
  CF :: Float -> Val Float
  CI :: Int   -> Val Int


-- LIBRARY

-- Thinking about not exposing the constructors to the library, but
-- wrap them in primitive functions instead.  Some constructors need
-- to be hidden, e.g. the *Rep words, but it might also be good to not
-- have to distinguish between functions and constructors.  And lib
-- should never do interpretation (pattern matching).
mul :: forall t. Prim t => Term t -> Term t -> Term t ; mul = Op2 Mul
add :: forall t. Prim t => Term t -> Term t -> Term t ; add = Op2 Add


square :: forall t. Prim t => Term (t -> t)
square = Lam $ \x -> (mul x x)



-- EVALUATOR

-- Represent t as [t], an infinite list.

ceval :: forall t. Val t -> t
ceval (CF f) = f
ceval (CI i) = i

eval :: forall t. Term t -> [t]

eval2 f a b = zipWith f (eval a) (eval b)

eval (F f) = repeat f
eval (I i) = repeat i
eval (Op2 Add a b) = eval2 addPrim a b
eval (Op2 Mul a b) = eval2 mulPrim a b
eval (App (Lam f) a) = eval $ f a
eval (Tup2 a b) = zip (eval a) (eval b)

-- ListRep converts a Haskell list as a term.
eval (ListRep r) = r

-- Signals are then defined recursively
eval (Sig init (Lam next)) = o where
  (so, o) = unzip $ eval $ next $ ListRep ((ceval init) : so)


-- FIXME: transpose
--eval (Lam u) = u' where
--  u' i = eval $ u $ ListRep i


run :: Term (t -> t') -> [t] -> [t']
run (Lam f) i = eval $ f $ ListRep i

e p = take 10 $ eval p
test_eval1 = e $ App (Lam (\x -> Op2 Add x x)) (F 1)
test_eval2 = e $ Sig (CI 1) (Lam (\s -> (Tup2 (Op2 Add s s) s)))
  



-- COMPILERS

-- Tagged expression language, roughly mirroring Term.  For
-- explorations.  The useful target is Anf below.

data Tagged = TFloat Float
            | TInt Int
            | TOp2 Opcode2 Tagged Tagged
            | TApp Tagged Tagged
            | TLam Tagged Tagged
            | TVar Int
            | TSig Tagged Tagged
            | TTup2 Tagged Tagged
            deriving Show



-- Compile to intermediate Ins language using Writer+State monad.
type C = WriterT [(Tagged,Tagged)] (State Int)

run_comp m = runState (runWriterT m) 0

-- Writer is not used.
-- State is used for variable generation.
newCVar :: forall t. C (Term t)
newCVar = do
  n <- get ; put $ n + 1
  return $ VarRep n
varNum (VarRep n) = n

-- Go multi-stage here. First pass 'comp' converts from GADT syntax to
-- ADT tagged syntax.  Contains enough information to compile to C.
-- Second pass 'gcen' prints C macro invocations that can then be
-- implemented in A C companion file.


ccomp :: forall t. Val t -> Tagged
ccomp (CF f) = TFloat f
ccomp (CI i) = TInt i

comp :: forall t. Term t -> C Tagged
comp (F f) = return $ TFloat f
comp (I i) = return $ TInt i
comp (VarRep n) = return $ TVar n

comp (Tup2    a b) = comp2 TTup2      a b
comp (Op2 op2 a b) = comp2 (TOp2 op2) a b
comp (App     f a) = comp2 TApp       f a
  
-- Needs variable generation: new variable is stored with TLam form
-- and goes into f to get the substituted term.
comp (Lam f) = do
  v    <- newCVar
  expr <- comp $ f v
  return $ TLam (TVar $ varNum v) expr

-- Hide a state machine (init value and update function).  The state
-- machine body needs to be compiled and saved somewhere.  Since it's
-- not needed anymore it can go to a writer.
comp (Sig i u) = do
  u' <- comp u
  o  <- newCVar
  let o' = TVar $ varNum o
  let i' = ccomp i
  tell $ [(o', TSig i' u')]
  return o'

comp2 f a b = do
  a' <- comp a
  b' <- comp b
  return $ f a' b'

-- The writer has signal implementations, the val is the main expression.
data Program = Program [(Tagged,Tagged)] Tagged -- deriving Show
instance Show Program where
  show (Program sigs val) =
    (concat $ map showSig sigs) ++
    "// prog:\n" ++ (show val)

showSig (TVar out, (TSig init (TLam (TVar state) (TTup2 state' out')))) =
  "// sig:\n" ++
  "state_next_" ++ (show state) ++ " = " ++ (show state') ++ "\n" ++
  "out_"  ++ (show out) ++ " = " ++ (show out') ++ "\n"




c p = Program sigs val where
  ((val,sigs),_state) = run_comp $ comp p
  
test_comp1 = c $ App (Lam (\x -> Op2 Add x x)) (F 1)

test_comp2 = c $ (square :: Term (Float -> Float))

test_comp3 = c $ Sig (CF 0) $ Lam (\x -> (Tup2 x x))

test_comp4 = c $
  let s1 = Sig (CF 1) $ Lam (\x -> Tup2 (add x x) x)
      s2 = Sig (CF 2) $ Lam (\x -> Tup2 (mul x x) x)
  in s1 `add` s2



-- Compile to intermediate Ins language using Writer+State monad.
data ABinding = ABinding PrimType Int Anf   deriving Show
data AInit    = AInit PrimType Int AnfConst deriving Show

-- Use two writers.  One for intial values, one for commands.  The
-- state monad is for variable numbering.
type A = (WriterT [AInit]
         (WriterT [ABinding]
         (State Int)))
     

data AnfProg = AnfProg Anf [AInit] [ABinding]


-- Fully flattened function body.  No Lam/App/Sig: everything is inlined.
data AnfConst = AFloat Float | AInt Int deriving Show
data Anf = AConst AnfConst
         | AOp2 Opcode2 Anf Anf
         | AVar Int
         | ATup2 Anf Anf
         deriving Show



canf :: forall t. Val t -> AnfConst
canf (CF f) = AFloat f
canf (CI f) = AInt f




anf :: forall t. Term t -> A Anf
anf (F f) = return $ AConst $ AFloat f
anf (I i) = return $ AConst $ AInt i
anf (VarRep n) = return $ AVar n

anf (Tup2    a b)   = anf2 undefined    ATup2      a b
anf c@(Op2 op2 a b) = anf2 (typePrim c) (AOp2 op2) a b

-- Signal definitions can only take this form.  All Lam/App needs to
-- be eliminated by macro expansion.
anf (Sig state0 (Lam update)) = do
  state  <- newAVar
  let n = varNum state
  let t = typePrim state -- PFloat -- FIXME: where to get the type?
  tell [AInit t n $ canf state0]
  let (Tup2 state' out) = update state
  astate' <- anf state'
  aout <- anf out
  lift $ tell [ABinding t n astate']
  return aout


newAVarNum :: forall t. A Int
newAVarNum = do
  n <- get ; put $ n + 1
  return n

newAVar :: forall t. A (Term t)
newAVar = do
  n <- newAVarNum
  return $ VarRep n

-- The basic structure of the form is that operation results are
-- always bound to a variable.
anf2 t op a b = do
  a' <- anf a
  b' <- anf b
  let cmd = op a' b'
  n <- newAVarNum
  lift $ tell [ABinding t n cmd] 
  return $ AVar n

instance Show AnfProg where
  show (AnfProg val inits defs) =
    "// init\n" ++  concat (map showAnfInit inits) ++
    "// update\n" ++  concat (map showAnfDef defs) ++
    "return " ++ (showAnfExpr val) ++ "\n"

showAnfDef (ABinding t n expr) =
  (show t) ++ " r" ++ (show n) ++ " <- " ++ (showAnfExpr expr) ++ "\n"

showAnfInit (AInit t n const) =
  (show t) ++ " r" ++ (show n) ++ " <- " ++ (showAnfExpr $ AConst const) ++ "\n"

showAnfExpr (AConst (AFloat f)) = show f
showAnfExpr (AConst (AInt i)) = show i
showAnfExpr (AVar n) = "r" ++ (show n)
showAnfExpr (AOp2 op2 a b) = (show op2) ++ "(" ++ (showAnfExpr a) ++ "," ++ (showAnfExpr b) ++ ")"

a :: Term t -> AnfProg
a p = AnfProg val inits defs where
  initRegNum = 0 :: Int
  m_wws = anf p
  m_ws = runWriterT m_wws
  m_s = runWriterT m_ws
  (((val, inits), defs), _nextRegNum) = runState m_s initRegNum

test_anf1 = a $ Sig (CI 0) $ Lam (\x -> (Tup2 (add x x) x))

