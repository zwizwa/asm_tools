
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
data Binding e n = Binding n (e n)

instance (Show n, Show (e n)) => Show (Binding e n) where
  show (Binding n e) = show n ++ " = " ++ show e


data Array = Array String
instance Show Array where
  show (Array a) = a

-- An array slot.  For now index and value have the same type.
data ArrayRef n = ArrayRef Array n
instance Show n => Show (ArrayRef n) where
  show (ArrayRef a i) = show a ++ "[" ++ show i ++ "]"


-- A Grid expression is parameterized by an expression language e.
data Grid e n = Loop Array ArrayIndex [Binding e (ArrayRef n)]

instance (Show n, (Show (e (ArrayRef n)))) => Show (Grid e n) where
  show (Loop a i bs)  =
    "for(int " ++ show i ++ "=0; " ++ show i ++ "< nb_el(" ++ show a ++ "); " ++ show i ++ "++){\n"
    ++ concat (map binding bs) ++ "}\n"
    where
      binding b = "\t" ++ show b ++ "\n"


-- Loop variables
data ArrayIndex = ArrayIndex String

instance Show ArrayIndex where
  show (ArrayIndex i) = i



-- Expression compiler needs to support parameterized intermediate
-- binding.






val :: Grid Expr ArrayIndex
val = Loop (Array "c") (ArrayIndex "i")
  [Binding (ArrayRef (Array "c") i)
    (Op (ArrayRef (Array "a") i)
        (ArrayRef (Array "b") i))]
  where i = (ArrayIndex "i")


run_test = do
  appendFile "/home/tom/exo/ghcid/output.log" $ (show val) ++ "\n"




