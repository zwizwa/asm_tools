{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module MyHDLRun where

import Seq
import SeqTerm
import qualified MyHDL
import System.IO
import System.Process

-- Running MyHDL
run_process :: String -> IO (Either [[Int]] (String, String, String))
run_process hdl_module_name = do
  let cmd = "PYTHONPATH=myhdl python3 run_myhdl.py " ++
        hdl_module_name ++ " " ++ hdl_module_name ++ ".py"
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
  let pyCode = show $
               MyHDL.noOutputCheck $
               MyHDL.testbench name inSizes mod inputs
  writeFile (name ++ ".py") pyCode
  run_process name

test :: IO ()
test = m where 
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
  
    



-- An earlier version used CPython before to avoid intermediate files
-- by passing in code as a string and using 'exec'.  However,
-- inspect.py needs access to the source code of the mode _on disk_,
-- so there is really no point in going that route.  Parsing the
-- output is easy when the generation of the output is done with
-- parsing in mind.

-- See git history or haskell_leftovers project.  CPython is a little
-- think on the wrapping, so the example is kept for future reference.

