module Data.AsmTools.CmdLineTool where

import qualified Data.AsmTools.VCD as VCD
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8

run ("bin2vcd":args) = bin2vcd args
  
run args = do
  putStrLn "asm-tools:"
  print args

bin2vcd args@[inFile,outFile] = do
  inBytes <- ByteString.readFile inFile
  let vcd = VCD.fromByteString inBytes
  ByteString.writeFile outFile $ UTF8.fromString $ show vcd
