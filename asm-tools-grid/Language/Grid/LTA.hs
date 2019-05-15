
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

   
- LOCALIZE:

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

type Index = String
type Array = String

data Form = LoopLet Loop | PrimLet Let
data Loop = Loop Index Program
data Program = Program [Form]


data Let = Let Cell [Cell]
data Cell = Cell Array [Index]


instance Show Program where
  show p = showp 0 p

-- Just use parameter passing for indentation.
class ShowP t where
  showp :: Int -> t -> String

tabs 0 = ""
tabs n = "  " ++ (tabs $ n-1)
instance ShowP Cell where
  showp _ (Cell a is) = a ++ concat is

instance ShowP Let where
  showp n (Let c cs) =
    tabs n ++
    showp n c ++ " <-" ++ (concat $ map ((" " ++) . (showp n)) cs)
instance ShowP Loop where
  showp n (Loop i p) =
    tabs n ++
    i ++ ":\n" ++
    showp (n+1) p
    -- ++ tabs n ++ "end " ++ i ++ "\n"
instance ShowP Form where
  showp n (LoopLet f) = showp n f
  showp n (PrimLet f) = showp n f ++ "\n"
instance ShowP Program where
  showp n (Program fs) = concat $ map (showp n) fs


test_val' =
  Program $
  [LoopLet $
   Loop "i" $
   Program $
    [LoopLet $
     Loop "j" $
     Program $
     [PrimLet $ Let (Cell "C" ["i"]) [(Cell "A" ["i"]), (Cell "B" ["i"])],
      PrimLet $ Let (Cell "D" ["i"]) [(Cell "C" ["i"]), (Cell "C" ["i"])]]],
   LoopLet $
   Loop "i" $
   Program $
    [LoopLet $
     Loop "j" $
     Program $
     [PrimLet $ Let (Cell "E" ["i"]) [(Cell "C" ["i"]), (Cell "D" ["i"])]]]    

    ]
    
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

loop' i fs = LoopLet $ Loop i $ Program fs
let' c a b = PrimLet $ Let c [a,b]

test_val =
  Program $
  [loop' i [loop' j [let' (c i j) (a i j) (b i j)]],
   loop' i [loop' j [let' (d i j) (a i j) (c i j)]]]
  where [a,b,c,d] = map a2 ["A","B","C","D"]


-- Next: create an example for each transformation.

fuse (LoopLet (Loop i  (Program as)))
     (LoopLet (Loop i0 (Program bs))) =
  case i == i0 of
    True  -> Just (LoopLet (Loop i (Program (as ++ bs))))
    False -> Nothing

interchange (LoopLet (Loop i (Program [LoopLet (Loop j p)]))) =
  (LoopLet (Loop j (Program [LoopLet (Loop i p)])))
      
      
