{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module SeqPrim(
  seqADD, seqSUB, seqAND, seqEQU, seqIF, seqCONC, seqSLICE,
  seqInt, seqMemInit, seqMemUpdate,
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

type Mem s = STUArray s Int Int

seqMemInit :: ST s (Mem s)
seqMemInit = newArray (0, 256) 0  -- FIXME: size!

seqMemRd _ = return 0

seqMemUpdate :: Mem s -> (Int, Int, Int, Int) -> ST s Int
seqMemUpdate arr (wEn,wAddr,wData,rAddr) = do
  rData <- readArray arr rAddr
  case wEn of
    0 -> return ()
    _ -> writeArray arr wAddr wData
  return rData

seqInt :: Integer -> Int
seqInt = fromIntegral


-- Second stage: execute the generated Haskell code.
-- a: memory arrays (tuble of STUArray)
-- r: register state (tuple of Int)
-- i/o is collected in a concrete [] type to make it easier to handle.

seqRun ::
  (forall s. ([Mem s], rd, r, [Int]) -> ST s (rd, r, [Int]))
  -> [Int]
  -> (rd, r)
  -> [[Int]] -> [[Int]]
seqRun f ac (rd0, r0) i = 
  runST $ do
    a <- sequence $ [ seqMemInit | _ <- ac ]
    let u _ _ [] = return []
        u rd r (i:is) = do
          (rd',r',o) <- f (a, rd, r, i)
          os <- u rd' r' is
          return (o:os)
    u rd0 r0 i


-- seqRun = undefined

-- For ST, it is important to understand which s parameters are
-- specific, and which are generic.
-- https://stackoverflow.com/questions/34494893/how-to-understand-the-state-type-in-haskells-st-monad
