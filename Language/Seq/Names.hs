{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.Seq.Names where
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- FIXME: find better names!

named m = m >>= named'
named' :: Exp -> ExpQ
named' lambda@(LamE [ListP args] var) = expr where
  expr = [| ( $(names) , $(return lambda) ) |]
  names = lift $ map name args
  name (VarP n) = nameBase n

names m = m >>= names'
names' :: Exp -> ExpQ
names' (ListE args@(arg:_)) = return $ expr arg where
  expr a@(VarE n) =
    -- NoBindS $
    AppE (AppE (VarE $ mkName "named") a)
    (LitE $ StringL $ nameBase n)
  expr a = error $ "Names.names: " ++ show a

-- tom@tp:~$ ghci -XTemplateHaskell
-- GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
-- Prelude> :m + Language.Haskell.TH

-- Prelude Language.Haskell.TH> runQ [| \[a,b] -> a |]
-- LamE [ListP [VarP a_1,VarP b_2]] (VarE a_1)


