-- RTL code tends to be "algebraic".  I.e. all the action happens
-- within the domain of a type.  Apart from enforcing basic code
-- structure, types do not help.  Tests are needed to constrain
-- behavior.
-- 
-- QuickCheck is used to raise the abstraction level of tests.
-- However, do note that in practice it is easier to work in steps,
-- i.e. graduate from "there exists" to "for all":
--
-- . Create a trace wrapper (t_) to transform the abstract (r S)
--   functions into the Int domain.
--
-- . From the trace wrapper, reate a single example (x_), that prints
--   out a human-readable report about how the function is performing.
--
-- . Once this works, generalize the example into property tests (p_)
--   to add test coverage, and comment out the verbose example.


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

import Seq
import SeqLib
import SeqEmu

import Data.Char
import Data.Bits
import Data.List
import Data.List.Split
import Test.QuickCheck hiding ((.&.),(.|.))

qc str f = do
  putStrLn $ "-- " ++ str
  quickCheck f

printL l = sequence $ map print l
printC l = sequence $ zipWith f l [0..] where
  f l' n  = do
    putStrLn $ "-- " ++ show n
    printL l'
  
main = do
  -- print $ toWord [1,0,0,0]
  -- print $ toBitList 4 8
  -- print $ downSample' $ t_clocked_shift 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0,0,0,0,1]]
  qc "p_bits" p_bits
  qc "p_sample" p_sample
  qc "p_clocked_shift" p_clocked_shift
  -- x_async_receiver_sample
  x_async_receiver


-- FIXME: Make better generators for fixed bit size sequences.

-- Tests for library code.
--    t_  Trace wrapper
--    x_  Example printout
--    p_  Property test


-- clocked_shift

p_clocked_shift :: NonNegative Int -> Positive Int -> [Int] -> Bool
p_clocked_shift (NonNegative sub) (Positive nb_bits) ints = p1 where

  p1 = wordSeq == wordSeq'

  wordSeq  = map (mask nb_bits) ints
  bitSeq   = toBits nb_bits wordSeq
  ins      = upSample' (cycle [sub `rem` 5]) $ map (:[]) bitSeq
  outs     = t_clocked_shift nb_bits ins
  wordSeq' = map head $ downSample' outs

t_clocked_shift :: Int -> [[Int]] -> [[Int]]
t_clocked_shift nb_bits = trace [1,1] $ \[bc,bv] -> do
  (wc, wv) <- clocked_shift ShiftLeft (SInt (Just nb_bits) 0) (bc, bv)
  return [wc, wv]


-- async_receiver_sample

t_async_receiver_sample nb_bits = trace [1] $ \[i] -> do
  -- use debug version which dumps internal state
  sample <- d_async_receiver_sample nb_bits i
  return sample

x_async_receiver_sample = do
  putStrLn "-- x_async_receiver_sample"
  printC $ chunksOf 8 $ t_async_receiver_sample 8
    [[i] | i <- rle (1,[8,8*9,8])]

t_async_receiver nb_bits = trace [1] $ \[i] ->
  d_async_receiver nb_bits i

x_async_receiver = do
  putStrLn "-- x_async_receiver"
  let
    -- ins = map ord "ABCDEF"
    ins = [0,7..255]
    out = t_async_receiver 8 $ map (:[]) $ uartBits 8 ins
    sample [v,i,bc,wc@1,state,count] = Just v
    sample _ = Nothing
    out' = downSample sample out
  -- printC $ chunksOf 8 out
  print $ out'
  








-- Tools

-- Defaults use "natural bit order", which places MSB on the left,
-- which makes list form, scope display and normal digit display.

toBitList :: Int -> Int -> [Int]
toBitList nb_bits val = map ((.&. 1) . (shiftR val)) $ reverse [0..nb_bits-1]

toWord :: [Int] -> Int
toWord bits = foldr f 0 $ reverse bits where
  f bit accu = (bit .&. 1) .|. (shiftL accu 1)

toBits :: Int -> [Int] -> [Int]
toBits nb_bits = concat . (map $ toBitList nb_bits)

toWords :: Int -> [Int] -> [Int]
toWords nb_bits = (map toWord) . (chunksOf nb_bits)

int2bool 0 = False
int2bool 1 = True
bool2int False = 0
bool2int True = 1

rle (val,ns) = f (int2bool val) ns where
  f _ [] = []
  f v (n:ns) = (replicate n $ bool2int v) ++ f (not v) ns

mask nb_bits v = v .&. msk where
  msk = (1 `shiftL` nb_bits) - 1

uartBits :: Int -> [Int] -> [Int] 
uartBits n str = samps where
  bits = concat $ map toBits str
  toBits w = [1,0] ++ (reverse $ toBitList n w) ++ [1,1]
  samps = upSample (\_ a -> a) (cycle [n-1]) bits

-- Bus sequences.

-- To keep things simple, use [] as signal container for input and
-- output busses.

-- In that setting, subsampled signals are simplest to represent by
-- pushing/poping the enable bit to/from the bus list.

upSample' :: [Int] -> [[Int]] -> [[Int]]
upSample' spaces = upSample en spaces where
  en True  a = (1:a)
  en False a = (0:a)

downSample' :: [[Int]] -> [[Int]]
downSample' = downSample sel where
  sel (1:a) = Just a
  sel (0:a) = Nothing

trace ::
  [Int]
  -> ([R S] -> M [R S])
  -> [[Int]] -> [[Int]]
trace inputBitSizes fm ins =
  iticks (onInts inputBitSizes fm) ins




-- Tools tests

-- downSample is left inverse of upSample
-- Test this separately as it is used in other tests.
p_sample :: [(NonNegative Int, Int)] -> Bool
p_sample spec = seq == seq' where
  spaces = map (getNonNegative . fst) spec
  seq    = map ((:[]) . snd) spec
  seq'   = downSample' $ upSample' spaces seq


p_bits :: Positive Int -> [Int] -> Bool
p_bits (Positive nb_bits) ints = p1 && p2  where
  
  p1 = words == words'
  p2 = bits  == bits'

  words  = map (mask nb_bits) ints
  bits   = toBits  nb_bits words
  words' = toWords nb_bits bits
  bits'  = toBits  nb_bits words'


