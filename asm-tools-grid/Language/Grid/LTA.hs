
{-

Loop transformation algebra

This is a mini-language used to illustrate loop transformations,
stripped from all non-essentials such as primtive operations and
loop/grid sizes.

These can then be lifted to langauges with more annotation.

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

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Grid.LTA where

import Data.Foldable
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State

-- The central data type: nested loops of ANF / SSA sections.
type Form' = Form Let'

-- The representation is split into container,,
data Form b = LetPrim b
            | LetLoop Index [Form b]
            deriving (Functor, Foldable, Traversable)

-- .. and contained type, which itself is split into a container of
-- cell references.
data Let c = Let c [c] deriving (Functor, Foldable, Traversable)
type Let' = Let Cell

-- .. and a cell type.
data Cell = Cell Grid [Index]
type Index = String
type Grid  = String


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
foldForm :: ([Form b] -> a')    -- foldList
         -> (b -> a)            -- letPrim
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

instance Show Cell where
  show (Cell a is) = a ++ concat is

instance ShowP b => ShowP (Form b) where
  showp n (LetLoop i p) =
    tabs n ++
    i ++ ":\n" ++
    showp (n+1) (Program p)
  showp n (LetPrim b) =
    tabs n ++ showp n b ++ "\n"

instance Show c => ShowP (Let c) where
  showp n (Let c cs) =
    show c ++ " <-" ++ (concat $ map ((" " ++) . show) cs)
  
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
      True  -> (LetLoop i (fs1 ++ fs2)) : t'
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
  intermediate' (ctx, l@(Let c@(Cell a is) cs)) =
    case escapes a ctx of
      False -> Just a
      True  -> Nothing

-- .. then modify the array dimensionality in a next step.
eliminate p = fmap (fmap txCell) p where
  isIntermediate a = elem a $ intermediates p
  txCell c@(Cell a is) =
    if isIntermediate a then (Cell a []) else c


-- FIXME: It's probably possible to do that circularly instead of in
-- two passes.


-- ESCAPE ANALYSIS

-- Note that there is no context available in the Functor / Foldable /
-- Traversable instances.  Since the standard container view is so
-- convenient, we stick to it as the main abstraction, and implement a
-- single annotation function that tags each element with its context.
annotate :: Program b -> Program (Context b, b)

-- Each element has two pieces of information attached: the loop
-- nesting context describing the current cell to be updated in the
-- current block and the order in which the loops are nested, and the
-- "execution stack" which describes the future of the sequential
-- execution.
type Stack b   = [[Form b]]
type Context b = ([Index],Stack b)

-- Note that it is more convenient to define a single traversal
-- routine that annotates both pieces of information, and define
-- projections that strip away the unneeded data, e.g.
annotate_i = (fmap (\((i,_),b) -> (i,b))) . annotate
annotate_s = (fmap (\((_,s),b) -> (s,b))) . annotate


-- To implement the annotation, a Reader is used to contain the
-- current context during traversal.  The code is split up into the
-- concrete annotation wrapper..
annotate p = p' where
  p' = runReader (annotate' pushi pushp add_context p) ([],[]) 

  pushp p = withReader (\(is, ps) -> (is, p:ps))
  pushi i = withReader (\(is, ps) -> (i:is, ps))

  add_context b = do
    ctx <- ask
    return (ctx, b)

-- ..and the abstract traversal.  The traversal itself is the mutual
-- recursion pattern associated directly with the 4 constructors,
-- sprinkled with pushi, pushp to accumulate context data that can then
-- picked up by f.
annotate' pushi pushp f (Program p) = fmap Program $ forms p where

  form (LetPrim b) = do
    b' <- f b
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


-- Given (Context Let, Let) at each binding site, the context can be
-- used to perform escape analysis.

-- An array escapes the current block if it is referenced after the
-- current block has finished executing.  This can be expressed in
-- terms of the execution stack.
escapes :: Grid -> Context Let' -> Bool
escapes a (_,(_:future_after_current)) =
  referenced a $ concat future_after_current

-- To check referencing, check each primtive's dependency list.  Note
-- that this has quadratic complexity in the most commmon case: a
-- temporary, non-escaping binding, as the entire future needs to be
-- traversed to determine that there are no references.  Is there a
-- better way?
referenced :: Grid -> [Form Let'] -> Bool
referenced a fs = or $ map checkPrim prims where
  prims = toList $ Program $ fs
  checkPrim (Let _ cs) = or $ map checkCell cs
  checkCell (Cell a' _) = a' == a








-- TEST

a1 a o   = Cell a [i]
a2 a i j = Cell a [i,j]

i = "i"
j = "j"

loop' i fs = LetLoop i fs
let' c a b = LetPrim $ Let c [a,b]

test_val' =
  Program $
  [loop' i [loop' j [let' (c i j) (a i j) (b i j),
                     let' (d i j) (c i j) (a i j)]],
   loop' i [loop' j [let' (e i j) (a i j) (d i j)]]]
  where [a,b,c,d,e] = map a2 ["A","B","C","D","E"]

-- test_val = fuse $ fuse test_val'
-- test_val = toList test_val'


-- test_val = annotate_i test_val'
-- test_val = intermediates test_val'
test_val = eliminate test_val'
