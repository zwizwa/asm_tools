{-# LANGUAGE Rank2Types #-}

-- Misc collection of sequence testing tools.
module Language.Seq.Test.Tools where

import qualified Language.Seq as Seq
import qualified Language.Seq.Term as SeqTerm
import qualified Language.Seq.Lib as SeqLib
import qualified Language.Seq.Emu as SeqEmu

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
    

-- Expand a time-stamped value-change list to equidistant sampling.
expandVC :: (Num n, Ord n) => n -> [(n,t)] -> [(n,t)]
expandVC period [] = []
expandVC period (first:rest) = first : f first rest where
  f _ [] = []
  f prev@(t_prev, v_prev) lst@(new@(t_new, _) : lst') =
    let t_now = t_prev + period
        same  = (t_now, v_prev)
    in if t_new > t_now then
      -- The value for t_now is not present, so keep constant
      same : f same lst
    else
      new  : f new lst'




mask nb_bits v = v .&. msk where
  msk = (1 `shiftL` nb_bits) - 1

uartBits :: Int -> [Int] -> [Int] 
uartBits factor str = samps where
  bits = concat $ map toBits str
  toBits w = [1,0] ++ (reverse $ toBitList 8 w) ++ [1,1]
  samps = upSample factor bits


-- This upsamples by 2 to accomodate the clock toggles.
-- Terminology is the one used in the Linux kernel.
-- See also SeqLib.hs


spiBits :: SeqLib.SpiMode -> [Int] -> [(Int,Int)]
spiBits _ [] = [] -- degenerate QC case
spiBits mode bits@(bit0:_) = p $ SeqLib.spi_mode mode where
  -- Different clock and data shifts
  _01@(_:_10) = cycle [0,1]
  _bb  = upSample 2 bits
  _xbb = 0 : _bb  -- extra 1/2 clock cycle!

  -- (cpol,cpha)
  p (0,0) = zip _01 _bb
  p (0,1) = zip _01 _xbb
  p (1,0) = zip _10 _bb
  p (1,1) = zip _10 _xbb
  

  
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

-- Special case: downsample list with clock, data as first 2 elements.
downSample' :: [[Int]] -> [[Int]]
downSample' = downSample sel where
  sel (1:a) = Just a
  sel (0:a) = Nothing

-- More convenient.  Get rid of downSample'
downSampleCD bus = map head $ downSample' bus

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

printL' :: Show n => [n] -> IO ()
printL' = putStr . concatLinesRLE . (map show)


list2 (a,b) = do
  return [a,b]


-- Format a signal table.

showSignals' :: Show t => Bool -> [String] -> [[t]] -> String
showSignals' doRle header signals = str where
  table = padTable (header : (map (map show) signals))
  (header':signals') = map (concat . (intersperse " ")) table
  sep = replicate (length header') '-'
  str = concatLines' $ header':sep:signals'
  concatLines' = if doRle then concatLinesRLE else concatLines
      
concatLinesRLE l = concatLines $ map showRLE $ rle l
concatLines = concat . (map (++ "\n")) 

showRLE (1, line) = line
showRLE (n, line) = line ++ " (" ++ show n ++ "x)"
  

    

showSignals :: Show t => [String] -> [[t]] -> String
showSignals = showSignals' False

padTable :: [[String]] -> [[String]]
padTable rows = prows where
  columns  = transpose rows
  widths   = map (maximum . (map length)) columns
  pcolumns = zipWith pad widths columns
  pad n column = map padCell column where
    padCell str@(c:_) =
      if isDigit c then pad ++ str else str ++ pad where
        n' = n - length str
        pad = replicate n' ' '
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
  



-- Trace writes to the CPU bus.
dbg_trace = bus_trace "bus_dbg"

bus_trace write (names, signals) =
  downSampleCD $ selectSignals [write, "bus_data"] names signals


-- Common staged cosimulation test bench:
-- 1. Create input/output pair in Seq
-- 2. Use it to properly instantiate a "port" module

testbench ::
  String
  -> [Int]
  -- Rank 2, because we instantiate it twice.
  -> (forall m r. Seq.Seq m r => [r Seq.S] -> m [r Seq.S])
  -> [[Int]]
  -> ([String], [Seq.SType], [SeqTerm.R Seq.S] -> SeqTerm.M (), [[Int]])

testbench name inSizes mod input = rv where
  rv = (portNames, portTypes, mod', take nb_ticks output)

  nb_ticks = length input
  nb_in    = length inSizes
  inTypes  = map SeqLib.bits inSizes
  
  -- Emulation.  This also gives us the output size.
  output = SeqEmu.iticks (SeqEmu.onInts inSizes mod) input
  nb_out = length $ head output

  -- Module code gen.  Adapt to pyModule interface.
  outTypes  = [Seq.SInt Nothing 0 | _ <- [1..nb_out]]
  portTypes = inTypes ++ outTypes
  portNames = ["p" ++ show n | n <- [0..(length portTypes)-1]]
  mod' ports = do
    let (ins,outs) = splitAt nb_in ports
    outs' <- mod ins
    sequence_ $ zipWith Seq.connect outs outs'




