-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module SeqPrim(
  seqADD, seqSUB,
  seqInt, seqInitMem, seqUpdateMem,
  seqPrimRun
  ) where
import Data.IntMap.Strict
import Data.Bits

trunc :: Int -> Int -> Int
trunc bits = (.&. mask) where
  mask = (1 `shiftL` bits) - 1
trunc2 op bits a b = trunc bits $ op a b

type Mem = IntMap Int
type SeqPrim2 = Int -> Int -> Int -> Int

seqADD = trunc2 (+)
seqSUB = trunc2 $ \a b -> a - b



seqInitMem :: Mem
seqInitMem = empty

seqUpdateMem :: ((Int, Int, Int, Int), Mem) -> (Int,  Mem)
seqUpdateMem ((wEn,wAddr,wData,rAddr), mem) = (rData, mem') where
  rData = findWithDefault 0 rAddr mem
  mem' = case wEn == 0 of
           True  -> mem
           False -> insert wAddr wData mem

seqInt :: Integer -> Int
seqInt = fromIntegral

-- Use same type as returned by compilation.
seqPrimRun ::
  ((m, r, [Int]) -> (m, r, [Int]),
   (m, r))
  -> [[Int]] -> [[Int]]
seqPrimRun (f, (m0, r0)) is = u m0 r0 is where
  u _ _ [] = []
  u m r (i:is) = (o : u m' r' is) where
    (m',r',o) = f (m,r,i)

