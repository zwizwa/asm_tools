{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.EDSP where

data S = S

class Monad m => EDSP m r | r -> m, m -> r where
  add :: r S -> r S -> m (r S)
