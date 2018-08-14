{-# LANGUAGE OverloadedStrings #-}

module MyHDLRun where

import qualified MyHDL

import qualified CPython as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Reflection as Py
import qualified CPython.System as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Exception as Py
import qualified CPython.Types.Module as Py

import qualified Control.Exception as Exception
import Data.Text(Text,pack)
import Data.Maybe
import Control.Monad
import System.IO



-- For reference. Use CPython instead.

-- import System.Process

-- -- Running MyHDL
-- run_process :: String -> IO ()
-- run+process funName = do
--   let cmd = "PYTHONPATH=myhdl python3 run_myhdl.py " ++
--         funName ++ " " ++ funName ++ ".py"
--   putStrLn "cmd:"
--   putStrLn cmd
--   (_, Just stdout, Just stderr, _) <-
--     createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
--   out <- hGetContents stdout
--   err <- hGetContents stderr

--   putStrLn "stdout:"
--   putStr out
--   putStrLn "stderr:"
--   putStr err
  
--   hClose stdout
--   return ()





-- Binary interface

-- Note that inspect.py needs access to the source file through a
-- getsourcefile() call, so we write it out.

-- https://stackoverflow.com/questions/12072252/


test_py = do
  let
    name = "x_tb"
    ins = map (:[]) [1,0,1,1,0,0]
    mod i = do return i
    pyCode = show $ MyHDL.testbench name [1] mod ins
  rv <- run_testbench (pack name) (pack pyCode)
  print $ fromJust $ rv

run_testbench :: Text -> Text -> IO (Maybe [[Int]])
run_testbench mod_name mod_text = do

  Py.initialize -- idempotent

  let catch :: Py.Exception -> IO (Maybe [[Int]])
      catch exc = do
        Py.print (Py.exceptionValue exc) stdout
        return Nothing
      str = (fmap Py.toObject) . Py.toUnicode
      attr o a = Py.getAttribute o =<< Py.toUnicode a
      call fun margs = do
        args <- sequence margs
        Py.callArgs fun args

      -- A rare occasion where point-free form is more readable.
      castInt = fromIntegral' <=< Py.fromInteger <=< fromJust' <=< Py.cast
      castList cast' = (sequence . (map cast')) <=< Py.fromList <=< fromJust' <=< Py.cast

      fromIntegral' = return . fromIntegral
      fromJust'     = return . fromJust
  
  Exception.handle catch $ do

    -- CPython.System.setPath is not platform-independent, so leave this in.
    sys <- Py.importModule "sys"
    sys_path <- attr sys "path"
    sys_path_append <- attr sys_path "append"
    call sys_path_append [str "."]
    call sys_path_append [str "myhdl"]
    
    lib <- Py.importModule "run_myhdl"
    run <- attr lib "run_text"
    busses <- call run [str mod_name, str mod_text]

    -- Py.print busses stdout
    busses <- castList (castList castInt) busses 
    return $ Just busses

