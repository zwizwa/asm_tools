import Language.Seq
import qualified Language.Seq.VerilogRun as VerilogRun

main = do
  out <- v_add $ map (:[]) [0..10]
  print out

v_add = VerilogRun.trace [8] $ \[i] -> do
  o <- add i 1
  return [o]

