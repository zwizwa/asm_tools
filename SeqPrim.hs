-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module SeqPrim(
  seqADD, seqSUB,
  seqInt, seqInitMem, seqUpdateMem
  ) where
import Data.IntMap.Strict
import Data.Bits

trunc :: Int -> Int -> Int
trunc bits = (.&. mask) where
  mask = (1 `shiftL` bits) - 1
trunc2 op bits a b = trunc bits $ op a b

seqADD = trunc2 (+)
seqSUB = trunc2 $ \a b -> a - b


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

