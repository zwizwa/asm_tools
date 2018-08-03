{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module SeqPrim(
  seqADD, seqSUB, seqAND, seqEQU, seqIF, seqCONC, seqSLICE,
  seqInt, seqInitMem, seqUpdateMem,
  seqRun
  ) where
import Data.IntMap.Strict
import Data.Bits
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

type T = Int

trunc :: T -> T -> T
trunc bits = (.&. mask) where
  mask = (1 `shiftL` bits) - 1

-- op1 :: (T -> T)           -> T -> T -> forall s. (ST s T)
-- op2 :: (T -> T -> T)      -> T -> T -> T -> forall s. (ST s T)
-- op3 :: (T -> T -> T -> T) -> T -> T -> T -> T -> forall s. (ST s T)

op1 :: (T -> T)           -> T -> T -> ST s T
op2 :: (T -> T -> T)      -> T -> T -> T -> ST s T
op3 :: (T -> T -> T -> T) -> T -> T -> T -> T -> ST s T
  
op1 op bits a     = return $ trunc bits $ op a
op2 op bits a b   = return $ trunc bits $ op a b
op3 op bits a b c = return $ trunc bits $ op a b c

-- seqADD   :: Monad m => T -> T -> T -> m T
-- seqSUB   :: Monad m => T -> T -> T -> m T
-- seqAND   :: Monad m => T -> T -> T -> m T
-- seqEQU   :: Monad m => T -> T -> T -> m T
-- seqSLICE :: Monad m => T -> T -> T -> m T
-- seqIF    :: Monad m => T -> T -> T -> T -> m T
-- seqCONC  :: Monad m => T -> T -> T -> T -> m T

seqADD = op2 (+)
seqSUB = op2 $ \a b -> a - b
seqAND = op2 (.&.)
seqIF  = op3 $ \c y n -> if c == 0 then n else y
seqEQU = op2 $ \a b   -> if a == b then 1 else 0

seqCONC  = op3 $ \bs a b -> (a `shiftL` bs) .|. b
seqSLICE = op2 $ shiftR

seqInitMem :: IntMap Int
seqInitMem = empty

seqUpdateMem :: ((Int, Int, Int, Int), IntMap Int) -> (Int,  IntMap Int)
seqUpdateMem ((wEn,wAddr,wData,rAddr), mem) = (rData, mem') where
  rData = findWithDefault 0 rAddr mem
  mem' = case wEn == 0 of
           True  -> mem
           False -> insert wAddr wData mem

seqInt :: Integer -> Int
seqInt = fromIntegral


-- Second stage: execute the generated Haskell code.
-- m: memory state (tuple of IntMap Int)
-- r: register state (tuple of Int)
-- i/o is collected in a concrete [] type to make it easier to handle.

-- Error about skolem variable: something tries to unify but is bound
-- at some other place.

-- seqRun ::
--   forall m r s.
--   ((m, r, [Int]) -> ST _ (m, r, [Int]),
--    (m, r))
--   -> [[Int]] -> [[Int]]


-- seqRun ::
--   (forall s. (m,r,[Int]) -> ST s (m, r, [Int])
--   ,(m,r)) -> [[Int]] -> [[Int]]
seqRun (f, (m0, r0)) is = runST $ u m0 r0 is where

  -- u :: m -> r -> [[Int]] -> ST s [[Int]]
  -- f' :: ()
  f' (m,r,(i:_)) = do
    v <- SeqPrim.seqADD 1 i i
    return (m, r, [v])

  u _ _ [] = return []
  u m r (i:is) = do
    (m',r',o) <- f' (m,r,i)
    os <- (u m' r' is)
    return (o:os)


-- Derive the correct form by adding stuff to runST.
seqRun' f i = runST $ f i
seqRun' :: (i -> forall s. ST s a) -> i -> a 


-- seqRun = undefined

-- For ST, it is important to understand which s parameters are
-- specific, and which are generic.
-- https://stackoverflow.com/questions/34494893/how-to-understand-the-state-type-in-haskells-st-monad
