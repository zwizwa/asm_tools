{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Default primitives for SeqTH
-- Note that SeqTH programs just capture lexical variables

module Language.Seq.Prim(
  seqADD, seqSUB, seqAND, seqOR, seqXOR, seqINV,
  seqEQU, seqIF, seqCONC, seqSLICE,
  seqInt, seqMemInit, seqMemUpdate,
  seqRun, seqRunOuts, seqRunMems, seqRunProbes,
  Mem
  ) where
  
import Data.Bits
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as Lazy
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Unsafe

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

type STMem s = STUArray s Int Int
type Mem     = UArray Int Int

seqMemInit :: Int -> (Int -> Int) -> ST s (STMem s)
seqMemInit addrBits init = do
  let size  = 1 `shiftL` addrBits
      inits = map init [0 .. size-1]
  newListArray (0, size) inits

seqMemRd _ = return 0

seqMemUpdate :: STMem s -> Int -> (Int, Int, Int, Int) -> ST s Int
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
  (forall s. ([STMem s], rd, r, [Int]) -> ST s (rd, r, [Int]))
  -> [Int]
  -> (rd, r)
  -> [String]
  -> [Int -> Int]
  -> [[Int]] -> ([String], ([Mem], [[Int]]))
seqRun update memSpec (rd0, r0) probeNames memInits i = (probeNames, out) where
  out = runST $ do
    as <- sequence $ zipWith seqMemInit memSpec memInits
    let u _ _ [] = return []
        u rd r (i:is) = do
          (rd',r',o) <- update (as, rd, r, i)
          os <- u rd' r' is
          return (o:os)
    os <- u rd0 r0 i
    as' <- sequence $ map unsafeFreeze as
    return (as', os)
    


-- Similar, but return the state of the memories.  The idea is to turn
-- this into a chunked run such that it can be turned into a lazy
-- operation again.

-- FIXME: This is not quite enough.  To return the final state, the
-- output needs to be collected differently.  Maybe best to collect it
-- in an array as well.  Also, output types are unknown.

seqRunProbes :: ([String], ([Mem], [[Int]])) -> [String]
seqRunOuts :: ([String], ([Mem], [[Int]])) -> [[Int]]
seqRunMems :: ([String], ([Mem], [[Int]])) -> [Mem]
seqRunProbes = fst
seqRunOuts = snd . snd
seqRunMems = fst . snd

seqRun' ::
  (forall s. ([STMem s], rd, r, [Int]) -> ST s (rd, r, [Int]))
  -> [Int]
  -> (rd, r)
  -> [String]
  -> [Int -> Int]
  -> [[Int]] -> ([String], ([Mem], [[Int]]))
seqRun' update memSpec (rd0, r0) probeNames memInits i = (probeNames, out) where
  out = runST $ do
    as <- sequence $ zipWith seqMemInit memSpec memInits
    let u _ _ [] = return []
        u rd r (i:is) = do
          (rd',r',o) <- update (as, rd, r, i)
          -- I want os and the : to be lazy.  The update above could be strict.
          os <- u rd' r' is
          return (o:os) 
    outs <- u rd0 r0 i
    as' <- sequence $ map unsafeFreeze as
    -- Do not mutate the as after unsafeFreeze
    return (as', outs)

-- mem2memInit :: Mem -> Int -> Int
-- mem2memInit = (!)

-- This hints at a better solution.  There is a way to have a lazy ST, and a way to wrap the arrays.
-- See Lazy.strictToLazyST
-- https://stackoverflow.com/questions/24072934/haskell-how-lazy-is-the-lazy-control-monad-st-lazy-monad
-- https://groups.google.com/forum/#!topic/comp.lang.haskell/JhUCLePEuQg
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad-ST-Lazy.html#g:2
