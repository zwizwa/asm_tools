
{-

A language for loops over grids.

The idea started out as an extension of Seq, which is inherently
_causal_, e.g. leaving time implicit.

It became apparent that the core of the problem should be reframed
spatially.  One way to look at it:

Seq  = Expr x Time
Grid = Expr x Space 

Where Expr is a language of scalar expressions that is oblivious about
grids.

Essentially, Grid hosts Expr in an explicit way, providing both scalar
input (references to bound array cells), and a holes to store Expr
output.

( Later on, it should be possible to transform Seq to Grid, e.g. to
implent it. )

To keep things simple, Grid does not have a way to represent scalar
values.  All values live inside a grid.  Scalar values are introduced
in a second compilation stage, based on usage patterns.  It is this
insight that separates it from a previous attempt to implement as
imilar idea (RAI writtin in Racket).


The simplest form of a grid expression looks like this when mapped
onto C:

for (int i=0; i<N; i++) {
    c[i] = F(a[i], b[i]);
}

Here c[i] is a the hole provided to the expession F, and a[i] and b[i]
are scalar inputs obtained by referencing the grid in some way.

Later on, more general array referencing should be possible.


The main optimization to be perform is this one:

1. Grid's representation using full intermediate arrays:

for (int i=0; i<N; i++) {
    c[i] = F(a[i], b[i]);
}
for (int i=0; i<N; i++) {
    d[i] = G(c[i]);
}

2. GridFuse's representation, re-using intermediate variables after
fusing loops:

for (int i=0; i<N; i++) {
    T c = F(a[i], b[i]);
    d[i] = G(c)
}

The purpose of this module is to make that transformation simple to
represent.



An imporantant complication is that Grid will be responsible for
providing the intermediate storage for the expression language's ANF
form.

TODO:
- Create an ANF expression compiler
- Parameterize "hole creation"
- Unify the resulting language with operations defined on Grid.LTA


-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}  -- FIXME: how to remove?

module Language.Grid where


-- A simple expression language with only a single 2-op primtiive to
-- illustrate the point.
data Expr n = Op n n

instance Show n => Show (Expr n) where
  show (Op a b) = "op(" ++ show a ++ ", " ++ show b ++ ")"

-- A central player is the binding, which associates a storage node to
-- the result of primitive expression evaluation.
data Let e n = Let n (e n)

instance (Show n, Show (e n)) => Show (Let e n) where
  show (Let n e) = show n ++ " = " ++ show e

data Arr = Arr String
instance Show Arr where
  show (Arr a) = a

-- An array slot.  For now index and value have the same type.
data Ref n = Ref Arr n
instance Show n => Show (Ref n) where
  show (Ref a i) = show a ++ "[" ++ show i ++ "]"



-- A Grid expression is parameterized by an expression language e.
data Grid e n = Loop Int [Arr] Idx [Let e (Ref n)]

instance (Show n, (Show (e (Ref n)))) => Show (Grid e n) where
  show (Loop n as i bs)  =
    concat (map declaration as)
    ++ "for(int " ++ show i ++ "=0; " ++ show i ++ "< " ++ show n ++ "; " ++ show i ++ "++){\n"
    ++ concat (map binding bs) ++ "}\n"
    where
      binding b = "\t" ++ show b ++ "\n"
      declaration a = "T " ++ show a ++ "[" ++ show n ++ "];\n"


-- Loop variables
data Idx = Idx String

instance Show Idx where
  show (Idx i) = i



-- Expression compiler needs to support parameterized intermediate
-- binding.




-- This expresses the construction of the array "c", but what I want
-- is the construction of the intermediates as well.  Essentially, the
-- Grid language needs to create all primtiive outputs, including
-- intermediates.

test_val :: Grid Expr Idx
test_val = Loop 100 [Arr "c", Arr "d"]  i
  [Let (Ref (Arr "c") i) (Op (Ref (Arr "a") i) (Ref (Arr "b") i)),
   Let (Ref (Arr "d") i) (Op (Ref (Arr "c") i) (Ref (Arr "c") i))]
  where i = (Idx "i")

