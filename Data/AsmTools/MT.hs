-- Some Monad Transformers

module Data.AsmTools.MT where

import Control.Monad.Trans

-- Assembler.

-- Re-inventing the writer monad as an exercise.2

-- FIXME: Is it generally so that a monad transformer has the to-wrap
-- monad on the outside?

newtype Asm i m t = Asm (m ([i], t))

instance Monad m => Functor (Asm i m) where
  fmap f (Asm m) = Asm $ do (is, v) <- m ; return (is, f v)
instance Monad m => Applicative (Asm i m) where
  pure v = Asm $ pure ([], v)
  (Asm mf) <*> (Asm mv) = Asm $ do
    (is, f) <- mf
    (is', v) <- mv
    return (is ++ is', f v)
instance Monad m => Monad (Asm i m) where
  (Asm mv) >>= f = Asm $ do
    (is, v) <- mv
    let Asm mv' = f v
    (is', v') <- mv'
    return (is ++ is', v')
instance MonadTrans (Asm i) where
  lift m = Asm $ do
    v <- m
    return ([], v)
  
test :: IO ()
test = do
  return ()


-- Now, instead of reimplementing The WriterT part, what about just
-- stacking state and writer, and exposing it as a single monad
-- transformer?

