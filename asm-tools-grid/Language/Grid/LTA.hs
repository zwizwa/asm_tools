
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

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}

module Language.Grid.LTA where

import Data.Foldable
import Data.Maybe
import Data.Char
import Control.Monad.Reader
-- import Control.Monad.State

import Language.Grid.StateCont

-- The central data type: nested loops of ANF / SSA sections.
type Form' = Form Let'

-- The representation is split into container,,
data Form b = LetPrim b
            | LetLoop Index [Form b]
            deriving (Functor, Foldable, Traversable)

-- .. and contained type, which itself is split into a container of
-- cell references.
data Let c = Let c [c]
           | Ret [c]
  deriving (Functor, Foldable, Traversable)
type Let' = Let Cell

-- .. and a cell type.
data Cell = Cell Grid [Ref]


-- Distinguish the variable from the index operation (derived from
-- variable).
data Index = Index Int deriving Eq
data Ref = Ref     Index
         | BackRef Index -- Backwards reference for accumulators

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

instance Show Cell where
  show (Cell a is) = show a ++ (concat $ map show is)

instance Show Index where
  show (Index i) = [chr (ord 'i' + i)]

instance Show Grid where
  show (Grid n) = [chr (ord 'A' + n)]

instance Show Ref where
  show (Ref v) = show v
  show (BackRef v) = show v ++ "'"

instance ShowP b => ShowP (Form b) where
  showp n (LetLoop i p) =
    tabs n ++
    show i ++ ":\n" ++
    showp (n+1) (Program p)
  showp n (LetPrim b) =
    tabs n ++ showp n b ++ "\n"

instance Show c => ShowP (Let c) where
  showp n (Ret cs)   =  "ret" ++ showArgs cs
  showp n (Let c cs) =  show c ++ " <-" ++ showArgs cs

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
  intermediate' (ctx, Let c@(Cell a is) cs) =
    case escapes a ctx of
      False -> Just a
      True  -> Nothing
  intermediate' (ctx, Ret _) = Nothing


-- .. then modify the array dimensionality in a next step.
eliminate p = fmap (fmap txCell) p where
  isIntermediate a = elem a $ intermediates p
  txCell c@(Cell a is) =
    if isIntermediate a then (Cell a []) else c


-- FIXME: It's probably possible to do that circularly instead of in
-- two passes.



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
  checkPrim p = or $ map checkCell $ refs p
  checkCell (Cell a' _) = a' == a
  refs (Let _ cs) = cs
  refs (Ret cs) = cs


-- ACCUMULATOR DISCOVERY

-- Similar to how escape analysis can identify grid dimensions that
-- can be flattened into a local variable, accumulator discovery
-- identifies dimensions in escaping variables that can be flattened
-- because they are used only as accumulators.


-- ...




-- MONAD FORM

-- Represent the language in Monad form.

-- The state-continuation monad is a good model for language with
-- nested scope and sharing.  However, like any abstraction based on
-- continuations, it takes some getting used to.

-- The continuation component allows the abstraction of the variable
-- binding mechanism, while the state component allows threading of a
-- dictionary.

-- See StateCont.hs for a direct implementation (not using monad
-- transformers).  SC takes 3 parameters.  The "user interface" is
-- just the monadic type:
type M t = SC S R t

-- The threaded state S is bookkeeping state used by the compiler.  In
-- our case, just the register allocator count for grids and loop
-- variables.
type S = (Int,Int)

-- and the return type is the value of the top level monadic
-- expression, which is a compiled program.
type R = [Form']


-- Converting the monadic form to a program then boils down to
-- providing the root continuation, and a seed for the state.
runSC (SC comp) = Program $ comp (0,0) k where
  k s v = [LetPrim $ Ret v]



-- Primitives then need to be implemented in CPS form


-- To implement variable binding we implement variable allocation and
-- binding insertion separately.  Variable allocation is
-- straightforward...
grid = SC comp where
  comp (g, i) k = k s' r where
    r = Cell (Grid g) []  -- Allocate a new grid register
    s' = (g + 1, i)       -- Update the allocator state

-- .. but let insertion is the tricky bit, as it uses the continuation
-- in a non-trivial way.
let' r args = SC comp where
  comp s k = (LetPrim $ Let r args) : k s r 

-- These two then combine into the user interface.
op args = do
  r <- grid
  let' r args

 
-- Similar for loop nesting there is loop variable allocation..
index = SC comp where
  comp (g, i) k = k s' r where
    r = Index i       -- Allocate a new grid register
    s' = (g, i + 1)   -- Update the allocator state

-- .. and the nesting primitive.  FIXME: This forks state.  There is
-- currently no way around that.
loop' i (SC comp) = SC comp' where
  comp' s k = (LetLoop i fs : k s vs) where
    fs = comp s k'
    k' s v = [LetPrim $ Ret v]
    (LetPrim (Ret vs) : _) = reverse fs

loop f = do
  i <- index
  loop' i $ f i

get = SC comp where
  comp s k = k s s



-- Example
p :: Cell -> Cell -> M [Cell]
p a b = do
  e <- loop $ \i -> do
    loop $ \j -> do
      c <- op [a,b]
      d <- op [a,c]
      return [d]
  loop $ \j -> do
    c <- op e
    d <- op [a,c]
    return e

testSC =
  runSC $ do
  a <- grid
  b <- grid
  p a b

  
        
-- TEST





-- Some notational shortcuts.  The data structures are optimized for
-- analysis, not necessarily for creating examples.

-- Notation: make it monadic, such that <- can be used for bindings.

ref  a is = Cell (Grid a) $ map Ref is
ref' a is = Cell (Grid a) $ map BackRef is

[a,b,c,d,e] = map ref [0,1,2,3,4]
[a'] = map ref' [0]


i  = Index 0
j  = Index 1

loop_ i fs = LetLoop i fs
let_ c a b = LetPrim $ Let c [a,b]

test_val' =
  Program $
  [loop_ i [loop_ j [let_ (c[i,j]) (a[i,j]) (b[i,j]),
                     let_ (d[i,j]) (c[i,j]) (a[i,j])]],
   loop_ i [loop_ j [let_ (e[i,j]) (a[i,j]) (d[i,j])]]]


test_val'' =
  Program $
  [loop_ i [let_ (a[i]) (a'[i]) (b[i])],
   loop_ j [let_ (d[j]) (a[i])  (c[j])]]




-- test_val = fuse $ fuse test_val'
-- test_val = toList test_val'


-- test_val = annotate_i test_val'
-- test_val = intermediates test_val'
-- test_val = eliminate test_val'

-- test_val = test_val''


test_val = testSC
