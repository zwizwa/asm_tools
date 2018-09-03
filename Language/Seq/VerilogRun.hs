--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Language.Seq.VerilogRun where

import Language.Seq
import Language.Seq.Term
import qualified Language.Seq.Verilog as Verilog

import System.IO
import System.IO.Error
import System.Process
-- import System.Socket
-- import System.Socket.Type.Stream
-- import System.Socket.Family.Unix

import System.Posix.Files
-- import Control.Concurrent
import Control.Exception

-- import Data.Bits

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as Char8
import Control.Monad



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


removeLink' p = do
  let handle e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
  removeLink p `catch` handle


testPipe = do
  let sock_path = "/tmp/seq_sock"
      cmd = "make -C ~/asm_tools/examples/verilog cosim"


  -- Set up socket.  Module will connect here.
  removeLink' sock_path
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix sock_path)
  listen sock maxListenQueue

  -- Start process
  (_, Just stdout, Just stderr, _) <-
    createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }

  -- Expecting only one connection from the module.
  (conn, _) <- accept sock

  msg <- recv conn 1024
  putStrLn $ "msg: " ++ show msg

  send conn msg

  msg <- recv conn 1024
  putStrLn $ "msg: " ++ show msg
  
  close conn
  close sock

  out <- hGetContents stdout
  err <- hGetContents stderr

  putStrLn "stderr:"
  putStr err
  putStrLn "output:"
  putStr out

  removeLink sock_path

