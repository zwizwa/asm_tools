{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module SeqPrim(
  seqADD, seqSUB, seqAND, seqOR, seqXOR, seqINV,
  seqEQU, seqIF, seqCONC, seqSLICE,
  seqInt, seqMemInit, seqMemUpdate,
  seqRun
  ) where
import Data.Bits
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

type T = Int

trunc :: T -> T -> T
trunc (-1) = id
trunc bits = (.&. mask) where
  mask = (1 `shiftL` bits) - 1

op1 :: (T -> T)           -> T -> T -> ST s T
op2 :: (T -> T -> T)      -> T -> T -> T -> ST s T
op3 :: (T -> T -> T -> T) -> T -> T -> T -> T -> ST s T
  
op1 op bits a     = return $ trunc bits $ op a
op2 op bits a b   = return $ trunc bits $ op a b
op3 op bits a b c = return $ trunc bits $ op a b c

seqINV = op1 complement

seqADD = op2 (+)
seqSUB = op2 $ \a b -> a - b
seqAND = op2 (.&.)
seqOR  = op2 (.|.)
seqXOR = op2 xor
seqIF  = op3 $ \c y n -> if c == 0 then n else y
seqEQU = op2 $ \a b   -> if a == b then 1 else 0

seqCONC  = op3 $ \bs a b -> (a `shiftL` bs) .|. b
seqSLICE = op2 $ shiftR

type Mem s = STUArray s Int Int
type Mem'  = UArray Int Int

seqMemInit :: Int -> (Int -> Int) -> ST s (Mem s)
seqMemInit addrBits init = do
  let size  = 1 `shiftL` addrBits
      inits = map init [0 .. size-1]
  newListArray (0, size) inits

seqMemRd _ = return 0

seqMemUpdate :: Mem s -> Int -> (Int, Int, Int, Int) -> ST s Int
seqMemUpdate arr bits (wEn,wAddr,wData,rAddr) = do
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
  -> [String]
  -> [Int -> Int]
  -> [[Int]] -> ([String], [[Int]])
seqRun update memSpec (rd0, r0) probeNames memInits i = (probeNames, out) where
  out = runST $ do
    a <- sequence $ zipWith seqMemInit memSpec memInits
    let u _ _ [] = return []
        u rd r (i:is) = do
          (rd',r',o) <- update (a, rd, r, i)
          os <- u rd' r' is
          return (o:os)
    u rd0 r0 i


-- To run lazily, chunk the ST runs.  This requires dumping out state.
-- Simplest to do that by splitting state init and run.

-- Here's the core routine: return the array.

-- seqRunInit ::
--   (forall s. ([Mem s], rd, r, [Int]) -> ST s (rd, r, [Int]))
--   -> [Int]
--   -> (rd, r)
--   -> [String]
--   -> [Int -> Int]
--   -> [[Int]] -> ([Mem'], rd, r)
-- seqRunInit update memSpec (rd0, r0) probeNames memInits i =
--   runST $ do
--     as  <- sequence $ zipWith seqMemInit memSpec memInits
--     as' <- sequence $ map runSTUArray as
--     return (as', rd0, r0)
