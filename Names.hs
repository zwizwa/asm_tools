{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Names where
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

name :: Name -> ExpQ
name n = [| ( $(lift (nameBase n ++ " = ")) ++ show $(varE n) ) |]


nameFun m = m >>= nameFun'
nameFun' :: Exp -> ExpQ
nameFun' lambda@(LamE [ListP args] var) = expr where
  expr = [| ( $(names) , $(return lambda) ) |]
  names = lift $ map name args
  name (VarP n) = nameBase n


-- tom@tp:~$ ghci -XTemplateHaskell
-- GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
-- Prelude> :m + Language.Haskell.TH

-- Prelude Language.Haskell.TH> runQ [| \[a,b] -> a |]
-- LamE [ListP [VarP a_1,VarP b_2]] (VarE a_1)


