-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module SeqPrim(
  seqADD, seqSUB, seqAND, seqEQU, seqIF, seqCONC, seqSLICE,
  seqInt, seqInitMem, seqUpdateMem
  ) where
import Data.IntMap.Strict
import Data.Bits

trunc :: Int -> Int -> Int
trunc bits = (.&. mask) where
  mask = (1 `shiftL` bits) - 1
  
trunc1 :: (Int -> Int)               -> Int -> Int -> Int
trunc2 :: (Int -> Int -> Int)        -> Int -> Int -> Int -> Int 
trunc3 :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Int
trunc1 op bits a     = trunc bits $ op a
trunc2 op bits a b   = trunc bits $ op a b
trunc3 op bits a b c = trunc bits $ op a b c

seqADD = trunc2 (+)
seqSUB = trunc2 $ \a b -> a - b
seqAND = trunc2 (.&.)

seqIF  = trunc3 $ \c y n -> if c == 0 then n else y
seqEQU = trunc2 $ \a b   -> if a == b then 1 else 0

seqCONC  = trunc3 $ \bs a b -> (a `shiftL` bs) .|. b
seqSLICE = trunc2 $ shiftR

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


