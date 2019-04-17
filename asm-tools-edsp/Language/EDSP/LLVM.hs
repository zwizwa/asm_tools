{-# LANGUAGE CPP, OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}


module Language.EDSP.LLVM where


-- LLVM
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

-- Start from the llvm_simple example.  Create a tagless final wrapper
-- as soon as possible.  It's going to be necessary.

-- Create the monad.  For now it can just be the LLVM monad.

-- Split up in the different parts, exposing only the "pure" core on
-- the inside.

llvm_simple =
  ppllvm $ buildModule "exampleModule" $ llvm_function

llvm_function = mdo
  function "add" [(i32, "a"), (i32, "b")] i32 llvm_entry

llvm_entry [a, b] = mdo
  entry <- block `named` "entry"; do
    c <- llvm_pure [a, b]
    ret c

llvm_pure [a, b] = do
  c <- I.add a b
  return c

run = do
  T.writeFile "/tmp/test.ll" $ llvm_simple
  T.putStrLn $ llvm_simple
