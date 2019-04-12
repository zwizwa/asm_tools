--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Language.Seq.VerilogRun where

import Language.Seq(Seq,S)
import Language.Seq.Term
import qualified Language.Seq as Seq
import qualified Language.Seq.Verilog as Verilog
import qualified Language.Seq.Test.Tools as TestTools

import Control.Monad
import Data.Binary.Get
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as DBS

import System.IO
import System.IO.Error
import System.IO.Temp
import System.Process
import System.Environment

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NBS




-- We need to talk to code that runs inside of a VPI module.  There
-- are a couple of ways to do this, but the simplest one seems to be a
-- unix domain socket in a dedicated temporary directory.  The
-- location is then passed to the module code through an environment
-- variable.  Since we have that directory, why not put the generated
-- .v files there as well.


withTempDir = withTempDirectory "/tmp" "seq"
  
trace ::
  [Int]
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]] -> IO [[Int]]
  
trace inSizes mod inputs = withTempDir $ \dir -> do

  -- Convert in->out Seq-style function to explicit port module.  Also
  -- "probe" to determine output size.
  let name = "testbench" -- not really needed for anything
      in_probe = take 1 inputs
      (portNames, portTypes, mod', out_probe) =
        TestTools.testbench name inSizes mod in_probe
      nb_to_verilog = length inSizes
      nb_from_verilog = length $ head out_probe
      v_code = show $ Verilog.vModule' Verilog.Cosim name portNames portTypes mod'
      name' = dir ++ "/" ++ name
      v_file = name' ++ ".v"
      vvp_file = name' ++ ".vvp"
  -- putStrLn v_code
  writeFile v_file v_code

  -- There's no good way to hard-link this, so it's expected to be
  -- configured higher up.
  cosim <- getEnv "SEQ_COSIM"
  
  let sock_path = dir ++ "/sock"
      gen = "iverilog " ++ v_file ++ " -o " ++ vvp_file
      setVar = "SEQ_SOCK=" ++ sock_path
      start = setVar ++ " vvp -m" ++ cosim ++ " " ++ vvp_file
      cmd = gen ++ " ; " ++ start

  putStrLn setVar

  -- Set up listening socket.  Module will connect here.
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix sock_path)
  listen sock maxListenQueue

  -- Start process
  putStrLn cmd
  createProcess (shell cmd)

  -- Expecting only one connection from the module.
  putStrLn "Waiting for module to connect.."
  (conn, _) <- accept sock
  putStrLn "Connected."

  -- FIXME: For "real" simulations, it will make sense to push/pull
  -- chunks with larger granularity, which will limit the number of
  -- system calls.  For now, just do one sample at a time.
  
  let getFrom = do
        msg <- NBS.recv conn $ 4 * nb_from_verilog
        return $ runGet from $ DBS.fromStrict msg
      from =
        sequence $ [getWord32le | _ <- [1..nb_from_verilog]]

      putTo bus = do
        let msg = DBS.toStrict $ toLazyByteString $ mconcat $ map putWord32le bus
        NBS.send conn msg

      cleanup = do
        close conn
        close sock

      run = mapM tick inputs
      tick i = do
        o <- getFrom
        putTo $ map fromIntegral i
        return $ map fromIntegral o

  outputs <- run
  cleanup
  return outputs

