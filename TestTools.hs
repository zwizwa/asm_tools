-- Misc collection of sequence testing tools.
module TestTools where

import Data.Char
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Test.QuickCheck hiding ((.&.),(.|.))
import Test.QuickCheck.Gen hiding (bitSize, getLine)
import Language.Haskell.TH

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
uartBits factor str = samps where
  bits = concat $ map toBits str
  toBits w = [1,0] ++ (reverse $ toBitList 8 w) ++ [1,1]
  samps = upSample factor bits

-- Note that oversample is relative to the 2x oversampling already
-- needed to represent the clock.

spiBits :: Int -> [Int] -> [(Int,Int)]
spiBits factorDiv2 bits = out where
  sclk  = cycle [0,1]
  sdata = upSample 2 bits
  out   = upSample factorDiv2 $ zip sclk sdata

  
-- Bus sequences.

-- To keep things simple, use [] as signal container for input and
-- output busses.

-- In that setting, subsampled signals are simplest to represent by
-- pushing/poping the enable bit to/from the bus list.

upSample factor seq =  
  reSample (\_ a -> a) (cycle [factor]) seq

reSample' :: [Int] -> [[Int]] -> [[Int]]
reSample' spaces = reSample en spaces where
  en True  a = (1:a)
  en False a = (0:a)

downSample' :: [[Int]] -> [[Int]]
downSample' = downSample sel where
  sel (1:a) = Just a
  sel (0:a) = Nothing

-- Isolate (enable,value)
downSampleBus unpack = downSample sel where
  sel bus = case unpack bus of
    (1,v) -> Just v
    (0,_) -> Nothing


fracSample :: Double -> [t] -> [t]
fracSample inc bits = f 0 bits where
  f _ [] = []
  f p bs@(b:bs') =
     case p >= 1 of
       True  -> f (p - 1) bs'
       False -> b : f (p + inc) bs


-- Generic routines
-- Insert extra spaces in between samples allowing custom tagging
-- (e.g. insert an enable signal in some form).

reSample :: (Bool -> a -> b) -> [Int] -> [a] -> [b]
reSample en spaces as = concat $ zipWith dup spaces as where
  dup 0 _ = []
  dup n a = (en True a) : (map (en False) $ replicate (n-1) a)

downSample :: (b -> Maybe a) -> [b] -> [a]
downSample sel = catMaybes . (map sel)



-- th_

-- traceTH fm ins =
--   let p@(f,i@(mi,si)) = $(return $ SeqTH.seqLam $ SeqTerm.compile SeqTH.seqLamTest)
  


word :: Int -> Gen Int
word nb_bits = arbitrary >>= return . (mask nb_bits)

wordList :: Gen (Int,[Int])
wordList = do
  nb_bits <- choose (1,16)
  lst <- listOf $ word nb_bits
  return (nb_bits, lst)

listOfMaxSize :: Int -> Gen a -> Gen [a]
listOfMaxSize n gen = do
  k <- choose (0,n)
  vectorOf k gen

bitList :: Gen [Int]
bitList = do
  let f True  = 1
      f False = 0
  bools <- listOf $ arbitrary
  return $ map f bools
  

memZero = cycle [const (0 :: Int)]

qc str f = do
  putStrLn $ "-- " ++ str
  quickCheck f

qc' str f = do
  putStrLn $ "-- " ++ str
  verboseCheck f

printL l = sequence_ $ map print l
printC l = sequence_ $ zipWith f l [0..] where
  f l' n  = do
    putStrLn $ "-- " ++ show n
    printL l'
  

