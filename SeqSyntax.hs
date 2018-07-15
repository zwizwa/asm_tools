module SeqSyntax where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Free
import Data.Char
import SE

seqFile fileName = do
  contents <- runIO $ readFile fileName
  case readSE fileName contents of
    Left  msg    -> error msg
    Right parsed -> return $ expr parsed

-- Capture variables.  Provide context in a different way.

atom a@(a0:_) =
  case isDigit a0 of
    True  -> LitE $ IntegerL $ read a
    False -> VarE $ mkName a

expr (Free [Pure op, Pure a, Pure b]) =
  AppE (AppE (atom op) (atom a)) (atom b)

expr e = error $ "SeqSyntax.exp: " ++ show e



