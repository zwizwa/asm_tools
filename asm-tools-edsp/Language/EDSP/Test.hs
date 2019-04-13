{-# LANGUAGE CPP, OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Language.EDSP.Test where


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
import LLVM.IRBuilder.Instruction

llvm_simple =
  ppllvm $ buildModule "exampleModule" $ mdo
  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
    entry <- block `named` "entry"; do
      c <- add a b
      ret c


