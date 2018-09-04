--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Language.Seq.VerilogRun where

import Language.Seq
import Language.Seq.Term
import qualified Language.Seq.Verilog as Verilog

import System.IO
import System.IO.Error
import System.IO.Temp
import System.Process
-- import System.Socket
-- import System.Socket.Type.Stream
-- import System.Socket.Family.Unix

import System.Posix.Files
-- import Control.Concurrent
import Control.Exception

-- import Data.Bits

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NBS

import qualified Data.ByteString.Lazy as DBS

import Control.Monad

import Data.Binary.Get
import Data.Binary.Builder


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



-- We need to talk to code that runs inside of a VPI module.  There
-- are a couple of ways to do this, but the simplest one seems to be a
-- unix domain socket in a dedicated temporary directory.  The
-- location is then passed to the module code through an environment
-- variable.  Since we have that directory, why not put the generated
-- .v files there as well.

testPipe = run where
  run = run_testbench' "testPipe" [8] mod $ map (:[]) [0..10]
  mod [i] = do
    o <- add i 1
    return [o]
  
run_testbench' ::
  String
  -> [Int]
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]] -> IO (Either [[Int]] (String, String, String))
run_testbench' name inSizes mod inputs = withTempDirectory "/tmp" "seq" $ \dir -> do

  -- FIXME: this needs a different version of testbench'
  -- Currently it leads to:
  -- assign s2 = (p0 + s2);
  -- assign p1 = s2;

  
  let vCode = show $ Verilog.testbench' name inSizes mod inputs
  putStrLn vCode
  -- FIXME: verilog glue is missing
  writeFile (dir ++ "/" ++ name ++ ".v") vCode
  run_process name
  
  let sock_path = dir ++ "/sock"
      setVar = "SEQ_SOCK=" ++ sock_path
      cmd = setVar ++ " make -C ~/asm_tools/examples/verilog cosim"
      nb_from_verilog = 1
      nb_to_verilog = 1
      nb_out = 1

  putStrLn setVar

  -- Set up socket.  Module will connect here.
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix sock_path)
  listen sock maxListenQueue

  -- Start process
  (_, Just stdout, Just stderr, _) <-
    createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }

  -- Expecting only one connection from the module.
  (conn, _) <- accept sock

  -- FIXME: For "real" simulations, it will make sense to push/pull
  -- chunks with larger granularity, which will limit the number of
  -- system calls.  For now, just do one sample at a time.
  
  let getFrom = do
        msg <- NBS.recv conn $ 4 * nb_from_verilog
        let ws = runGet from $ DBS.fromStrict msg
        print ws
        return ws
      from = do
        ws <- sequence $ [getWord32le | _ <- [1..nb_from_verilog]]
        return $ ws -- map fromIntegral ws

      putTo bus = do
        let msg = DBS.toStrict $ toLazyByteString $ mconcat $ map putWord32le bus
        NBS.send conn msg


  getFrom ; putTo [7]
  getFrom ; putTo [8]
  getFrom
  
  close conn
  close sock

  out <- hGetContents stdout
  err <- hGetContents stderr

  putStrLn "stderr:"
  putStr err
  putStrLn "output:"
  putStr out

  removeLink sock_path

  -- FIXME
  return $ Left []


removeLink' p = do
  let handle e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
  removeLink p `catch` handle


{-

// TB needs an additional stub that
// - declares module i/o reg/wire
// - instantiates module under test
// - initial block with $seq_to() and $seq_from()
// - rst, clk drivers
// - always @(posedge clk) for $seq_tick

// How to do this without duplication?  Instead of generating a module
// signature, just generate plain registers and collect those in a
// $seq_to / $seq_from call.

module counter_tb;

   reg clk = 0;
   reg rst = 0;
   wire [7:0] count; 
   
   counter U0 (clk, rst, count);

   reg [3:0] v = 123;
   
   initial begin
      $seq_to(count);
      $seq_from(v);
      #1 rst <= 1;
      //repeat (20) @(posedge clk);
      //$finish;
   end
   
   always @(posedge clk) begin
      $display("%d %d", count, v);
      $seq_tick;
   end
   
   always 
     #5  clk = ~clk;

endmodule
-}
