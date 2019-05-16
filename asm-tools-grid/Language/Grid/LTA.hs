
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


module Language.Grid.LTA where

-- The central data type: nested loops of ANF sections.
data Form = LetPrim Cell [Cell]
          | LetLoop Index [Form]

-- Leaf nodes
type Index = String
type Array = String
data Cell = Cell Array [Index]

-- Just a behavioral wrapper for [] for external interfacing
-- (e.g. Show).  The 'Form' type uses [] directly to reduce
-- constructor wrapping clutter.
newtype Program = Program [Form]


-- The mutually recursive type is associated to mutually recursive
-- folds.  Express this by expressing both legs separately,
-- one parameterized by the other.

-- The first fold is primtiive and thus needs destructuring
foldForm :: ([Form] -> a')          -- foldList
         -> (Cell -> [Cell] -> a)   -- letPrim
         -> (Index -> a' -> a)      -- letLoop
         -> (Form -> a)
foldForm foldList letPrim letLoop = form where
  form (LetPrim c cs) = letPrim c cs
  form (LetLoop i fs) = letLoop i $ foldList fs

-- The second fold is just a modified list foldr
foldFormList :: (Form -> a')      -- foldForm
             -> (a' -> a -> a)    -- cons
             -> a                 -- nil
             -> [Form] -> a
foldFormList foldForm cons = foldr cons' where
  cons' h = cons (foldForm h)


-- These then combine through mutual recursion to create a fold that
-- treats all levels the same.
foldProgram letPrim letLoop cons nil (Program p) = foldFL p where
  foldFL = foldFormList foldF  cons nil
  foldF  = foldForm     foldFL letPrim letLoop 



  



instance Show Program where
  show p = showp 0 p

-- Just use parameter passing for indentation.
class ShowP t where
  showp :: Int -> t -> String

tabs 0 = ""
tabs n = "  " ++ (tabs $ n-1)

instance ShowP Cell where
  showp _ (Cell a is) = a ++ concat is

instance ShowP Form where
  showp n (LetLoop i p) =
    tabs n ++
    i ++ ":\n" ++
    showp (n+1) (Program p)
  showp n (LetPrim c cs) =
    tabs n ++
    showp n c ++ " <-" ++ (concat $ map ((" " ++) . (showp n)) cs)
    ++ "\n"
  
instance ShowP Program where
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
let' c a b = LetPrim c [a,b]

test_val' =
  Program $
  [loop' i [loop' j [let' (c i j) (a i j) (b i j)]],
   loop' i [loop' j [let' (d i j) (a i j) (c i j)]]]
  where [a,b,c,d] = map a2 ["A","B","C","D"]

test_val = fuse' $ fuse' test_val'

-- Transformations.

-- FUSE
--

-- Split into element-wise comparison and an iteration mechanism.
-- Note that this implicitly uses the name of the index parameter to
-- identify a particular loop range.

fuse (Program fs) =
  -- Note that this does only one level.  Split out multi-level
  -- transformations in a more traditional recursive fold.
  Program $ fuse_list fuse_forms fs

fuse_forms fa@(LetLoop i  as)
           fb@(LetLoop i0 bs) =
  case i == i0 of
    True  -> Just (LetLoop i (as ++ bs))
    False -> Nothing
fuse_forms fa fb = Nothing


fuse' p = Program $ foldProgram LetPrim LetLoop cons [] p where
  cons h@(LetLoop i as) t@((LetLoop i0 bs):t') =
    case i == i0 of
      True  -> (LetLoop i (as ++ bs)) : t'
      False -> h:t
  cons a b = a:b


interchange (LetLoop i [LetLoop j p]) =
  (LetLoop j [LetLoop i p])
      
      
-- eliminate is the difficult one, because it requires information
-- that is not local!


-- Iteration patterns


-- Elimination needs escape analysis.  In the current form


-- Generic iteration patterns.

fuse_list f = fuse where
  fuse (e1:e2:es) =
    case f e1 e2 of
      Just e  -> fuse (e:es)
      Nothing -> e1 : fuse (e2:es)
  fuse es = es


