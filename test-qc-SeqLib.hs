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
import Test.QuickCheck.Gen hiding (bitSize, getLine)


main = do

  -- print $ toWord [1,0,0,0]
  -- print $ toBitList 4 8
  -- print $ downSample' $ t_clocked_shift 4 $ [[1,i] | i <- [1,1,1,1,0,0,0,0,0,0,0,1]]

  qc "p_bits" p_bits
  qc "p_sample" p_sample
  qc "p_clocked_shift" p_clocked_shift
  qc "p_async_receiver" p_async_receiver
  -- x_async_receiver_sample
  x_async_receiver

  x_mem
  x_fifo



qc str f = do
  putStrLn $ "-- " ++ str
  quickCheck f

qc' str f = do
  putStrLn $ "-- " ++ str
  verboseCheck f

printL l = sequence $ map print l
printC l = sequence $ zipWith f l [0..] where
  f l' n  = do
    putStrLn $ "-- " ++ show n
    printL l'
  

-- FIXME: Make better generators for fixed bit size sequences.

-- Tests for library code.
--    t_  Trace wrapper
--    x_  Example printout
--    p_  Property test


-- clocked_shift


p_clocked_shift = forAll vars pred where
  vars = do
    sub <- choose (1,8)
    wl  <- wordList
    return (sub, wl)
  pred (sub, (nb_bits, words)) = words == words' where
    bits   = toBits nb_bits words
    ins    = upSample' (cycle [sub]) $ map (:[]) bits
    outs   = t_clocked_shift nb_bits ins
    words' = map head $ downSample' outs

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

p_async_receiver = forAll (listOf $ word 8) pred where
  pred words = words == words' where
    ins    = map (:[]) $ uartBits oversample words
    outs   = t_async_receiver nb_bits $ ins
    words' = downSample sample outs
    sample [v,i,bc,wc@1,state,count] = Just v
    sample _ = Nothing
  
  oversample = 8
  nb_bits = 8


t_mem = trace [8,1,8,8] $ \i@[ra,we,wa,wd] -> do
  t <- stype wd
  SeqEmu.closeMem [t] $ \[rd] ->
    return ([(we, wa, wd, ra)], (rd:i))

x_mem = do
  let writes = [[0,1,x,x+20] | x <- [1..10]]
      reads  = [[x,0,0,0]    | x <- [1..10]]
      outs = t_mem $ writes ++ reads
  putStrLn "-- x_mem rd,ra,we,wa,wd"
  printL outs


-- FIXME: how to abstract?
t_fifo = trace [1,1,8] $ \i@[re,we,wd] -> do
  -- t: type
  -- d: data
  -- a: address
  -- e: enable
  td <- stype wd
  let ta = SInt (Just 4) 0
  (wa,ra) <- closeReg [ta, ta] $ \[wa, ra] -> do
    wa1 <- inc wa
    ra1 <- inc ra
    ra' <- if' re ra1 ra
    wa' <- if' we wa1 wa
    return ([wa',ra'], (wa,ra))
  rd <- SeqEmu.closeMem [td] $ \[rd] -> do
    return ([(we, wa, wd, ra)], rd)
  return (rd:wa:ra:i)
  
x_fifo = do
  -- Write a bunch of data into the memory, then read it out.
  let writes = [[0,1,x] | x <- [1..10]]
      reads  = replicate 10 $ [1,0,0]
      outs = t_fifo $ writes ++ reads
  putStrLn "-- x_fifo rd,wa,ra,re,we,wd"
  printL outs


    




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
uartBits oversample str = samps where
  bits = concat $ map toBits str
  toBits w = [1,0] ++ (reverse $ toBitList 8 w) ++ [1,1]
  samps = upSample (\_ a -> a) (cycle [oversample]) bits

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


word :: Int -> Gen Int
word nb_bits = arbitrary >>= return . (mask nb_bits)

wordList :: Gen (Int,[Int])
wordList = do
  nb_bits <- choose (1,16)
  lst <- listOf $ word nb_bits
  return (nb_bits, lst)




-- Tools tests

-- downSample is left inverse of upSample
-- Test this separately as it is used in other tests.
p_sample = forAll vars pred where
  vars = sized $ \n -> do
    spaces <- vectorOf n $ choose (1,8)
    seq    <- vectorOf n $ arbitrary
    return (spaces, seq)
  pred (spaces, seq) = seq == seq' where
    seq' = downSample' $ upSample' spaces seq



p_bits = forAll wordList p where
  p (nb_bits, words) = p1 && p2 where
    
    p1 = words == words'
    p2 = bits  == bits'
  
    bits   = toBits  nb_bits words
    words' = toWords nb_bits bits
    bits'  = toBits  nb_bits words'
  
  
  
