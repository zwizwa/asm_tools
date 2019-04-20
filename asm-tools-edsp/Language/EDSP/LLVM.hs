-- TODO: This will need some focus on the structure of the monads.  I
-- want to have my own stack to be able to implement some EDSP
-- language context.  It seems that what is needed is to mix it with
-- the IRBuilderT transformer.

{-# LANGUAGE CPP, OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Language.EDSP.LLVM where



-- LLVM
import Data.Text.Lazy(Text)
import Data.Text.Lazy.IO as T
import Data.Text.Lazy.Encoding

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction as I


-- EDSP
import Language.EDSP

-- Implementation
import Control.Monad.State

-- Start from the llvm_simple example.  Create a tagless final wrapper
-- as soon as possible.  It's going to be necessary.

-- Create the monad.  For now it can just be the LLVM monad.

-- Split up in the different parts, exposing only the "pure" core on
-- the inside.

llvm_simple :: Text
llvm_simple =  ppllvm $ llvm_module

llvm_module :: Module
llvm_module =  buildModule "exampleModule" $ llvm_function

llvm_function :: ModuleBuilder Operand
llvm_function = mdo
  function "add" [(i32, "a"), (i32, "b")] i32 llvm_entry

llvm_entry :: Monad m => [Operand] -> IRBuilderT m ()
llvm_entry [a, b] = mdo
  entry <- block `named` "entry"; do
    c <- llvm_pure [a, b]
    ret c

-- The bridge to the LLVM monadic representation is the class
-- MonadIRBuilder, but it seems we can make it a little less abstract
-- by focusing only on.
llvm_pure :: MonadIRBuilder m => [Operand] -> m Operand
llvm_pure [a, b] = do
  c <- I.add a b
  return c


-- Use a custom monad to compile to LLVM.
newtype M t = M { unM :: State String t } deriving
  (Functor, Applicative, Monad)
-- instance MonadIRBuilder M where



run = do
  T.writeFile "/tmp/test.ll" $ llvm_simple
  T.putStrLn $ llvm_simple
