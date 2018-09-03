--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Language.Seq.VerilogRun where

import Language.Seq
import Language.Seq.Term
import qualified Language.Seq.Verilog as Verilog

import System.IO
import System.IO.Error
import System.Process

import System.Posix.Files
import Control.Concurrent
import Control.Exception

import Data.Bits


-- Running MyHDL
run_process :: String -> IO (Either [[Int]] (String, String, String))
run_process hdl_module_name = do
  let cmd = "iverilog " ++ hdl_module_name ++ ".v"
  (_, Just stdout, Just stderr, _) <-
    createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
  out <- hGetContents stdout
  err <- hGetContents stderr
  return $ case err of
    "" -> Left $ map read $ lines out
    _  -> Right (cmd, out, err)

-- Temp file is necessary.  See below.
run_testbench ::
  String
  -> [Int]
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]] -> IO (Either [[Int]] (String, String, String))
run_testbench name inSizes mod inputs = do  
  let vCode = show $ Verilog.testbench name inSizes mod inputs
  -- FIXME: concatenate testbench wrapper, or use script.x
  writeFile (name ++ ".v") vCode
  run_process name

testStdout :: IO ()
testStdout = m where 
  name = "x_tb"
  inputs = map (:[]) [1,0,1,1,0,0]
  inSizes = [1]
  mod [i] = do return [i]
  
  m = do
    result <- run_testbench name inSizes mod inputs
    case result of
      Left out -> print out
      Right (cmd, stdout, stderr) -> do
        putStrLn "cmd:"
        putStrLn cmd
        putStrLn "stderr:"
        putStr stdout
        putStrLn "output:"
        putStr stderr



-- Using binary pipe interface.
-- createNamedPipe

-- We can only open for writing ince the other end has opened the pipe
-- for reading, so loop until it's ready.

openPipeAppend p = again 3 where
  again 0 = error $ "openPipeAppend: " ++ p
  again n = do
    openFile p AppendMode `catch` handle where
      handle e
        | isDoesNotExistError e = do
            print e
            threadDelay 500000
            again $ n - 1
        | otherwise = throwIO e

      


testPipe = do
  let mode = ownerReadMode .|. ownerWriteMode .|. namedPipeMode
      p_to = "/tmp/seq_to"
      p_from = "/tmp/seq_from"
      cmd = "cat /dev/urandom"

  
  createNamedPipe p_to mode
  createNamedPipe p_from mode
  to <- openFile p_to ReadMode
  
  (_, Just stdout, Just stderr, _) <-
    createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }

  from <- openPipeAppend p_from

  removeLink p_to
  removeLink p_from

