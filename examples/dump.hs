import System.Environment
import Language.Seq.Test.Tools
import qualified Data.ByteString as B

main = do
  (file:op:args) <- getArgs
  str <- B.readFile file
  let f = case op of
            "d0" -> d0
            "d1" -> d1
      ints = map fromIntegral $ B.unpack str
  traverse print $ f ints args

d1 ints [] = do
  let f int = (toBits 8 [int], int)
  map f $ odds ints

d0 ints [] = do
  let f int = (toBits 8 [int], int)
  map f $ evens ints

evens (a:_:as) = a : evens as
evens _ = []

odds = evens . tail

  
