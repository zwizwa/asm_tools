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

-- I forgot why this is called rle.  Rename it
rle' (val,ns) = f (int2bool val) ns where
  f _ [] = []
  f v (n:ns) = (replicate n $ bool2int v) ++ f (not v) ns


-- This routine seems more useful for printing trace outputs
rle :: (Eq t, Integral i) => [t] -> [(i,t)]
rle [] = error "rle needs non-empty list"
rle (b:bs) = f 1 b bs where
  f n s [] = [(n,s)]
  f n s (b:bs) = case s == b of
    True  -> f (n+1) s bs
    False -> (n,s) : f 1 b bs
    



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
  


list2 (a,b) = do
  return [a,b]


-- Format a signal table.

showSignals' :: Bool -> [String] -> [[Int]] -> String
showSignals' doRle header signals = str where
  table = padTable (header : (map (map show) signals))
  (header':signals') = map (concat . (intersperse " ")) table
  sep = replicate (length header') '-'

  lines  = header':sep:signals'
  lines' = map rle' $ rle lines
  rle' (1, line) = line
  rle' (n, line) = line ++ " (" ++ show n ++ "x)"
  
  str = concat $ map (++ "\n") $ if doRle then lines' else lines
      
    

showSignals = showSignals' False

padTable :: [[String]] -> [[String]]
padTable rows = prows where
  columns  = transpose rows
  widths   = map (maximum . (map length)) columns
  pcolumns = zipWith pad widths columns
  pad n column = map padCell column where
    padCell str = lpad ++ str where
      n' = n - length str
      lpad = replicate n' ' '
  prows = transpose pcolumns


selectSignals columns names signals = signals' where
  signals' = map select signals
  select row = map (row !!) indices
  indices = map name2col columns
  name2col nm = fromJust' nm $ lookup nm $ zip names [0..]
  -- Assume that probe names are correct.  This is only supposed to be
  -- used for testing, so raise an error when a probe is not found.
  fromJust' _ (Just x) = x
  fromJust' nm Nothing = error $
    "showSelectSignals: " ++ show nm ++ " not found in " ++ show names
  
