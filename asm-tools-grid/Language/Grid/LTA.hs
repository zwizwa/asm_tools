
{-

Loop transformation algebra

This is a mini-language used to illustrate loop transformations,
stripped from all non-essentials such as primtive operations and
loop/grid sizes.

These can then be lifted to languages with more annotation.

These are the (bi-directional) operations in the notation developed in
rtl.txt, presented in the direction that is most common (i.e. is
actually an optimization).

- FUSE

i:
   Bi <- Ai
i:
   Ci <- Bi
=>
i:
   Bi <- Ai
   Ci <- Bi

   
- ELIMINATE:

i:
   Ci <- Ai Bi
   Di <- Ci Ci
=>
i:
   C  <- Ai Bi
   Di <- C  C 


- HOIST

i:
   C  <- A B
   Ei <- C Di 
=>
C <- A B
i:
   Ei <- C Di


- INTERCHANGE

i:
  j:
     Cij <- Aij Bij
=>
j:
  i:
     Cij <- Aij Bij


-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}

module Language.Grid.LTA where

import Data.Functor.Identity
import Data.Foldable
import Data.Maybe
import Data.Char
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Fail


-- The central data type: nested loops of ANF / SSA sections.
type Form' = Form Let'

-- The representation is split into container,,
-- b is binding type
data Form b =
  LetPrim b
  | LetLoop Index [Form b]
  deriving (Functor, Foldable, Traversable)

-- .. and contained type, which itself is split into a container of
-- cell references.
-- Fixme: binding and reference are not the same

type Opcode = String

-- i is grid index type
data Let i = Let (DefCell i) String [RefCell i]
           | Ret [RefCell i]
  deriving (Functor, Foldable, Traversable)
type Let' = Let Index

-- .. and a cell type.  This I found tricky to derive: it links the
-- Grid entity (e.g. grid name), tp[o the index variable type, and the
-- type of transformation perfomed on the index.

data Cell t i = Cell Grid [t i] deriving (Functor, Foldable, Traversable)
type DefCell  = Cell Def
type RefCell  = Cell Ref

-- A definition always uses unadulterated index variables.  I.e. you
-- can't just arbitrarily write into an array.  Note that this is just
-- the Identity functor with some printing attached to it.
data Def t = Def t deriving (Functor, Foldable, Traversable)

-- Distinguish the variable from the index operation (derived from
-- variable).
data Index = Index Int deriving Eq
type Ref'  = Ref Index

data Ref i = Ref i
           | BackRef i -- Backwards reference for accumulators
           deriving (Functor, Foldable, Traversable)
-- Note that only delay=1 accumulators are supported, but the code is
-- structured to later add full triangle coverage.


data Grid = Grid Int deriving Eq


-- This makes it all fit in a convenient type class hierarchy, which
-- simplifies substitution and analysiscode.


-- For convenience, add a wrapper to a bundle a complete a collection
-- of forms into a program.
data Program b = Program [Form b]
  deriving (Functor, Foldable, Traversable)



-- The generalized fold associated to the data type.  Note that this
-- is not the same as Foldable, which uses the list-like structure in
-- the Functor.  The generalized fold maps constructors to functions.


-- The type is a composition of two types: the prim/loop distinction
-- and use of [] to represent a sequence of bindings.  Implement
-- generalized folds for both components separately.

-- The first fold is primitive and thus needs destructuring
foldForm :: ([Form b] -> a')       -- foldList
         -> (b -> a)               -- letPrim
         -> (Index -> a' -> a)  -- letLoop
         -> (Form b -> a)
foldForm foldList letPrim letLoop = form where
  form (LetPrim p)     = letPrim p
  form (LetLoop i fs)  = letLoop i $ foldList fs

-- The second fold is a modified list foldr
foldFormList :: (Form b -> a')    -- foldForm
             -> (a' -> a -> a)    -- cons
             -> a                 -- nil
             -> [Form b] -> a
foldFormList foldForm cons = foldr cons' where
  cons' h = cons (foldForm h)


-- These then combine through mutual recursion.
foldProgram letPrim letLoop cons nil (Program p) = foldFL p where
  foldFL = foldFormList foldF  cons nil
  foldF  = foldForm     foldFL letPrim letLoop 




-- A Show instace to produce the notation used in the comments above.

instance ShowP b => Show (Program b)     where show p = showp 0 p
instance Show c => Show (Let c)          where show p = showp 0 p
instance ShowP (Form b) => Show (Form b) where show p = showp 0 p

instance (Show t, Show c) => ShowP (t, Let c)  where showp _ p = show p

class ShowP t where
  showp :: Int -> t -> String

tabs 0 = ""
tabs n = "  " ++ (tabs $ n-1)

instance Show (t i) => Show (Cell t i) where
  show (Cell a is) = show a ++ (concat $ map show is)

instance Show Index where
  show (Index i) = [chr (ord 'i' + i)]

instance Show Grid where
  show (Grid n) = [chr (ord 'A' + n)]

instance Show i => Show (Ref i) where
  show (Ref v) = show v
  show (BackRef v) = show v ++ "'"

instance Show i => Show (Def i) where
  show (Def v) = show v


instance ShowP b => ShowP (Form b) where
  showp n (LetLoop i p) =
    tabs n ++
    show i ++ ":\n" ++
    showp (n+1) (Program p)
  showp n (LetPrim b) =
    tabs n ++ showp n b ++ "\n"

instance Show c => ShowP (Let c) where
  showp n (Ret cs)   =  "ret" ++ showArgs cs
  showp n (Let c opc cs) =  show c ++ " <- " ++ opc ++ showArgs cs

showArgs cs = concat $ map ((" " ++) . show) cs
  
instance ShowP b =>  ShowP (Program b) where
  showp n (Program fs) = concat $ map (showp n) fs



-- The transoformations explained in the comments above.


-- FUSE
--
-- Fuse can be implemented as a generalized fold operation, where only
-- the list constructor is modified.  Note that this works bottom up,
-- so needs to be run multiple times to perform nested fusing.
--
fuse p = Program $ foldProgram LetPrim LetLoop cons [] p where
  cons h@(LetLoop i1 fs1) t@((LetLoop i2 fs2):t') =
    case i1 == i2 of
      True  -> (LetLoop i1 (fs1 ++ fs2)) : t'
      False -> h:t
  cons a b = a:b


-- INTERCHANGE
--
-- This is focused on a particular loop.  It needs some more context
-- to be applied properly.
--
interchange (LetLoop i [LetLoop j p]) = (LetLoop j [LetLoop i p])
interchange l = l




-- ELIMINATE

-- Eliminate requires escape analysis, which is non-local information.
-- Note that when we create a single form from a high level
-- description, it is known exactly which of the arrays are output and
-- which are temporary.  However, when we start fusing loops, this
-- information is no longer accurate, as an output of one state might
-- be fed into another state and become an intermediate value.  So we
-- do not bother tracking the original information and reconstruct it
-- instead.

-- Elimination works in two steps.  Create the elimination list..
intermediates p = catMaybes $ toList $ fmap intermediate' $ annotate p where
  intermediate' (ctx, Let c@(Cell a is) _ cs) =
    case escapes a ctx of
      False -> Just a
      True  -> Nothing
  intermediate' (ctx, Ret _) = Nothing


-- .. then modify the array dimensionality in a next step.
-- FIXME: This broke when refactoring types, but will no longer be
-- used in favor of gathering an indirect per array annotation
-- dictionary.
-- eliminate p = fmap (fmap txCell) p where
--   isIntermediate a = elem a $ intermediates p
--   txCell c@(Cell a is) =
--     if isIntermediate a then (Cell a []) else c
eliminate = undefined

-- 


-- CONTEXT ANNOTATION

-- Note that there is no context available in the Functor / Foldable /
-- Traversable instances.  Since the standard container view is so
-- convenient, we stick to it as the main abstraction, and provide a
-- mechanism that tags each element with its context.
annotate :: Program b -> Program (Context b, b)

-- Each element has two pieces of information attached: 1) the loop
-- nesting context, and 2) the "stack" associated to the sequential
-- exeuction context, describing what happens next.
type Stack b   = [[Form b]]
type Context b = ([Index],Stack b)

-- Note that it is more convenient to define a single traversal
-- routine that annotates everything we know, and define projections
-- that strip away the unneeded data, e.g.
annotate' proj p = (fmap (\(a,b) -> (proj a, b))) . annotate
annotate_i = annotate' fst
annotate_s = annotate' snd



-- To implement the annotation, a Reader is used to contain the
-- current context during traversal.  The code is split up into the
-- concrete annotation wrapper..
annotate p = p' where
  p' = runReader (annotateM pushi pushp add_context p) ([],[]) 

  pushp p = withReader (\(is, ps) -> (is, p:ps))
  pushi i = withReader (\(is, ps) -> (i:is, ps))

  add_context b = do
    ctx <- ask
    return (ctx, b)

-- ..and the abstract traversal.  The traversal itself is the mutual
-- recursion pattern associated directly with the 4 constructors,
-- sprinkled with pushi, pushp to accumulate context data that can then
-- picked up by f.
annotateM pushi pushp add_context (Program p) =
  fmap Program $ forms p where
  
  form (LetPrim b) = do
    b' <- add_context b
    return $ LetPrim b'

  form (LetLoop i p) = do
    p' <- pushi i $ forms p
    return $ LetLoop i p'

  forms [] = do
    return []

  forms (f:fs) = do
    f'  <- pushp fs $ form f
    fs' <- forms fs
    return (f':fs')


-- ESCAPE ANALYSIS

-- Given (Context Let, Let) at each binding site, the context can be
-- used to perform escape analysis.

-- An array escapes the current block if it is referenced after the
-- current block has finished executing.  This can be expressed in
-- terms of the execution stack.
escapes :: Grid -> Context Let' -> Bool
escapes a (_,(_:future_after_current)) =
  referenced a $ concat future_after_current
escapes _ _ = error "escapes: empty stack"

-- To check referencing, check each primtive's dependency list.  Note
-- that this has quadratic complexity in the most commmon case: a
-- temporary, non-escaping binding, as the entire future needs to be
-- traversed to determine that there are no references.  Is there a
-- better way?
referenced :: Grid -> [Form Let'] -> Bool
referenced a fs = or $ map checkPrim prims where
  prims = toList $ Program $ fs
  checkPrim p = or $ map checkCell $ cells p
  checkCell (Cell a' _) = a' == a
  cells (Ret rs)   = rs
  cells (Let _ _ rs) = rs
  

-- ACCUMULATOR DISCOVERY

-- Similar to how escape analysis can identify grid dimensions that
-- can be flattened into a local variable, accumulator discovery
-- identifies dimensions in escaping variables that can be flattened
-- because they are used only as accumulators.


-- ...



-- Tests

-- Note: 2019/6/2: Taking out the wrappers to generate the language.
-- For documentation purposes, it makes more sense to use the
-- constructors directly.

-- See Loop.hs for the parallel development of the monadic Language,
-- which currently needs to diverge structure-wise from what is in
-- here.

test_val =
  Program $
  [LetLoop (Index 0) $
   [LetPrim $
    Let (Cell (Grid 2) [Def $ Index 0])
    "opc"
    [Cell (Grid 0) [Ref $ Index 0],
     Cell (Grid 1) [Ref $ Index 0]
    ],
    LetPrim $
    Ret $ [Cell (Grid 2) [Ref $ Index 0]]
    ]]
  
