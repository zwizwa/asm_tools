{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module MyHDLRun where

import Seq
import SeqTerm
import qualified MyHDL
import System.IO
import System.Process

-- Running MyHDL
run_process :: String -> IO (String, String)
run_process hdl_module_name = do
  let cmd = "PYTHONPATH=myhdl python3 run_myhdl.py " ++
        hdl_module_name ++ " " ++ hdl_module_name ++ ".py"
  putStrLn "cmd:"
  putStrLn cmd
  (_, Just stdout, Just stderr, _) <-
    createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
  out <- hGetContents stdout
  err <- hGetContents stderr

  -- hClose stdout -- Don't. Gets closed on EOF.
  return (out,err)

-- Temp file is necessary.  See below.
run_testbench ::
  String
  -> [Int]
  -> (forall m r. Seq m r => [r S] -> m [r S])
  -> [[Int]] -> IO (String, [[Int]])
run_testbench name inSizes mod inputs = do  
  let pyCode = show $ MyHDL.noOutputCheck $ MyHDL.testbench name inSizes mod inputs
  writeFile (name ++ ".py") pyCode
  (out,err) <- run_process name
  let outs = map read $ lines out
  return (err, outs)
  

test :: IO ()
test = m where 
  name = "x_tb"
  inputs = map (:[]) [1,0,1,1,0,0]
  inSizes = [1]
  mod [i] = do return [i]
  m = do
    (err, output) <- run_testbench name inSizes mod inputs
    putStrLn "stderr:"
    putStr err
    putStrLn "output:"
    print output
  
    



-- This attempted to use CPython before to avoid intermediate files
-- using 'exec', but because inspect.py needs access to the source
-- code of the mode _on disk_, there is really no point in going that
-- route.  So we stick to just parsing the output of the testbench.

-- See git history or haskell_leftovers project.  CPython is a little
-- think on the wrapping, so the example is kept for future reference.

