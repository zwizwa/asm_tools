-- Before building an LLVM target, make a C target first.  The EDSP
-- language should be quite simple.  Also: maybe SEQ is already enough
-- actually?

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.EDSP.C where


-- EDSP
import Language.EDSP

-- Implementation
import Control.Monad.State


-- Use a custom monad to compile to LLVM.
newtype M t = M { unM :: State String t } deriving
  (Functor, Applicative, Monad)


run = do
  putStrLn "EDSP.C"
