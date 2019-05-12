-- Extension of Loop, with:
-- . nested bindings
-- . tuple nodes + dereference

module Language.Seq.TermLoop where

import Language.Seq.Term

run_test = do
  putStrLn "Language.Seq.TermLoop.run_test"
  putStrLn $ show 123


-- To start this out, create a toy language first.

-- Start from intermediate representation that can describe unfused
-- loops, with random index access.  Work at a level that can be
-- directly translated to C, to keep it real.

-- It is important to realize that Seq is too specific: it already has
-- incremental time steps, which is not what we want.




