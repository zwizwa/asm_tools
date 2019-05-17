
{-

Loop transformation algebra

This is a mini-language used to illustrate loop transformations,
stripped from all non-essentials such as primtive operations and
loop/grid sizes.

These can then be lifted to langauges with more annotation.

These are the (bi-directional) operations in the notation developed in
rtl.txt, in reducting order.

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
import Control.Monad.Reader
import Control.Monad.State

-- The central data type: nested loops of ANF sections.
type Form' = Form Let

-- The default binding type.  This is abstracted such that the Form
-- type can be a functor of bindings.
data Let = Let Cell [Cell]

data Form b = LetPrim b
            | LetLoop Index [Form b]
            deriving (Functor, Foldable, Traversable)




-- Leaf nodes
type Index = String
type Array = String
data Cell = Cell Array [Index]

-- Just a behavioral wrapper for [] for external interfacing
-- (e.g. Show).  The 'Form' type uses [] directly to reduce
-- constructor wrapping clutter.
data Program b = Program [Form b]
  deriving (Functor, Foldable, Traversable)

-- Generalized Fold (subsitute constructors).  Note that this is not
-- the same as Foldable, which uses the Functor structure of Form.

-- The mutually recursive type is associated to mutually recursive
-- folds.  Express this by expressing both legs separately,
-- one parameterized by the other.

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




instance ShowP b => Show (Program b)     where show p = showp 0 p
instance Show Let                        where show p = showp 0 p
instance ShowP (Form b) => Show (Form b) where show p = showp 0 p

instance Show t => ShowP (t, Let)        where showp _ p = show p

-- Just use parameter passing for indentation.
class ShowP t where
  showp :: Int -> t -> String

tabs 0 = ""
tabs n = "  " ++ (tabs $ n-1)

instance ShowP Cell where
  showp _ (Cell a is) = a ++ concat is

instance ShowP b => ShowP (Form b) where
  showp n (LetLoop i p) =
    tabs n ++
    i ++ ":\n" ++
    showp (n+1) (Program p)
  showp n (LetPrim b) =
    tabs n ++ showp n b ++ "\n"

instance ShowP Let where
  showp n (Let c cs) =
    showp n c ++ " <-" ++ (concat $ map ((" " ++) . (showp n)) cs)
    
  
instance ShowP b =>  ShowP (Program b) where
  showp n (Program fs) = concat $ map (showp n) fs


    
-- 1. language type class
-- 2. examples: nested "map" expressions
-- 3. processors on those expressions


-- m: code gen monad
-- s: symbol type
-- a: array type
--class Monad m => LTA m a s where
--  op ::

-- NOTE: Before doing the generic thing, first make a concrete
-- implementation, then generalize.

a1 a o   = Cell a [i]
a2 a i j = Cell a [i,j]

i = "i"
j = "j"

loop' i fs = LetLoop i fs
let' c a b = LetPrim $ Let c [a,b]

test_val' =
  Program $
  [loop' i [loop' j [let' (c i j) (a i j) (b i j)]],
   loop' i [loop' j [let' (d i j) (a i j) (c i j)]]]
  where [a,b,c,d] = map a2 ["A","B","C","D"]

-- test_val = fuse $ fuse test_val'
-- test_val = toList test_val'
test_val = annotate test_val'

-- Transformations.

-- FUSE
--
-- Can be implemented as a generalized fold operation, where only the
-- list constructor is modified.  Note that this works bottom up, so
-- needs to be run multiple times to perform nested fusing.
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




-- Annotate Functor elements with their path.  Note that this is an
-- operation that cannot be expressed by generic fold nor
-- Functor/Traversable.

annotate :: Program b -> Program ([Index], b)
annotate (Program fs) = Program $ forms [] fs where
  forms is fs = map (form is) fs
  form is (LetPrim b)    = LetPrim (is, b)
  form is (LetLoop i fs) = LetLoop i $ forms (i:is) fs


-- This can likely be expressed as a monadic traversal with a reader
-- constrant on the monad.  Is it necessary?


-- ELIMINATE

-- Eliminate requires escape analysis, which is non-local information.
-- Note that when we create the form, it is known exactly which of the
-- arrays are output and which are not.  However, when we start fusing
-- loops, this information is no longer accurate, as an output might
-- become an intermediate value.  So we do not bother tracking the
-- original information and reconstruct it instead.

-- An array A escapes a loop if it is referenced after the loop has
-- finished executing.  The make a decision about an array, we need to
-- look only at what happens in the future.

-- This can be done by 1) annotating each node with a zipper-style
-- (future) context, and 2) searching that future context for
-- references

-- To annotate, use a Reader monad. 

type Ctx b = ([Index],[[Form b]])
annotateZipper :: Program b -> Program (Ctx b, b)
annotateZipper (Program fs) = Program $ runReader (forms fs) ([],[]) where

  -- Use a composite context, because loop index nesting and form
  -- traversal nesting are updated independently.
  pushfs fs  = withReader (\(is, fss) -> (is, fs:fss))
  pushi  i   = withReader (\(is, fss) -> (i:is, fss))
  
  form (LetPrim b) = do
    ctx <- ask
    return $ LetPrim (ctx, b)

  form (LetLoop i fs) = do
    fs' <- pushi i $ forms fs
    return $ (LetLoop i) fs'

  forms [] = do
    return []

  forms (f:fs) = do
    f'  <- pushfs fs $ form f
    fs' <- forms fs
    return (f':fs')
    


