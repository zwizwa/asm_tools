{- Array constructor loops

The basic idea is to take a scalar ANF language like Seq, and extend
it with arrays definition loops and references, e.g.

loop :: (i -> M t) -> M (A t)
ref  :: (A t) -> i -> M t

where

M : DSL monad
A : Array type

Note that this also implements multi-dimensional arrays, as loop and
ref can be nested.


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

module Language.Grid.Loop where

import Data.Functor.Identity
import Data.Foldable
import Data.Maybe
import Data.Char
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Fail



-- Note that this language can represent "grid slices", i.e. partially
-- applied array references.  However they will not end up in the
-- final result that is compiled to C.  It is merely an intermediate
-- form to mesh better with the nested loops / nested data structure
-- representation in Haskell.

type Opcode = String
data Var = Var Int deriving Show

--                    dst
data Form   = LetOp   Var Opcode [Var]
            | LetRef  Var Var Var -- partial application
            | LetLoop Var Var [Form]
            deriving Show

type Env      = [Var]    -- Loop nesting environment
type Bindings = [Form]   -- Current bindings list
type ID       = Int      -- Allocator unique ID

newtype M t = M { unM :: ReaderT Env (WriterT Bindings (State ID)) t } deriving
  (Functor, Applicative, Monad,
   MonadReader Env,
   MonadWriter Bindings,
   MonadState  ID)

instance MonadFail M where
  fail str = error $ "Loop.M match error: " ++ str

runM :: M t -> Env -> ID -> ((t, Bindings), ID)
runM m env state = runState (runWriterT (runReaderT (unM m) env)) state

runM' m = fs where
  ((out,fs),state') = runM m [] 0
  

getID :: M ID
getID = do s <- get ; put $ s+1 ; return s
var   = fmap Var getID


op opc args = do
  v <- var
  -- e <- ask  -- FIXME: might not be necessary
  -- Store the definition in the dictionary,...
  tell $ [LetOp v opc args]
  -- ... but provide a reference to the program.  This allows
  -- operators on references to be used.
  return $ v

-- Array references are treated separately.
ref a i = do
  v <- var
  tell $ [LetRef v a i]
  return $ v



-- Note that if multiple return values are needed, just return an
-- array of a tuple or something, which is more honest anyway.

loop :: (Var -> M Var) -> M Var
loop f = do
  -- Run the subform, threading state and augmenting environment.
  i <- var ; is <- ask ; state <- get
  let ((out, fs), state') = runM (f i) (i:is) state
  -- Rethread the state
  put state'
  tell $ [LetLoop out i fs]
  return $ out


-- This needs to be defined for primtives and arrays.  FIXME: There is


op2 :: Opcode -> Var -> Var -> M Var
op2 opc a b = do
  r <- op opc [a,b]
  return r

ref2 a i j = do a1 <- ref a i ; ref a1 j

-- Example
-- p :: Cell -> Cell -> M' r [Cell]
p a b = do
  d <- loop $ \i -> do
    loop $ \j -> do
      aij <- ref2 a i j
      bij <- ref2 b i j
      c <- op2 "mul" aij bij
      d <- op2 "mul" aij c
      return d
  loop $ \i -> do
    loop $ \j -> do
      dij <- ref2 d i j
      c <- op2 "mul" dij dij
      e <- op2 "mul" c c
      return e

testM = runM' $ do
  a <- var
  b <- var
  p a b


{-

Not quite there yet.

[LetLoop (Var 9) (Var 2)
 [LetLoop (Var 9) (Var 3)
  [LetRef (Var 4) (Var 0) (Var 2),
   LetRef (Var 5) (Var 4) (Var 3),
   LetRef (Var 6) (Var 1) (Var 2),
   LetRef (Var 7) (Var 6) (Var 3),
   LetOp (Var 8) "mul" [Var 5,Var 7],
   LetOp (Var 9) "mul" [Var 5,Var 8]]],
 LetLoop (Var 15) (Var 10)
  [LetLoop (Var 15) (Var 11)
   [LetRef (Var 12) (Var 9) (Var 10),
    LetRef (Var 13) (Var 12) (Var 11),
    LetOp (Var 14) "mul" [Var 13,Var 13],
    LetOp (Var 15) "mul" [Var 14,Var 14]]]]

-}

    
-- TEST






test_val = testM

