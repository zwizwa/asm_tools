import Language.Seq
import Language.Seq.VerilogRun
main = do
  testPipe

testPipe = run where
  run = run_testbench "testPipe" [8] mod $ map (:[]) [0..10]
  mod [i] = do
    o <- add i 1
    return [o]

