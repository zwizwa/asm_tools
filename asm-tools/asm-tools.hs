import qualified Data.AsmTools.CmdLineTool as CmdLineTool
import System.Environment
main = do
  args <- getArgs
  CmdLineTool.run args

