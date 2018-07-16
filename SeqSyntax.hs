module SeqSyntax where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Free
import Data.Char
import SE

seqFile fileName = do
  -- addDependentFile fileName -- needs abs path
  contents <- runIO $ readFile fileName
  case readSE fileName contents of
    Left  msg    -> error msg
    Right parsed -> return $ expr parsed


atom a@(a0:_) =
  case isDigit a0 of
    -- Just capture variables.  Provide context in a different way.
    False -> VarE $ mkName a
    True  -> LitE $ IntegerL $ read a

-- TODO: Racket-style 2-pass: expand first to explicit %app, then
-- perform applications.

-- Normal application needs to do two things:
-- a) Convert to ANF, for do notation
-- b) Uncurry multi-arg functions

-- Conversion to ANF is simplest using the state-continuation monad.


expr (Free (Pure "some_macro" : _)) = error "NI: some_macro"
expr (Free ((Pure op) : as)) = app (atom op) as
expr e = error $ "SeqSyntax.exp: " ++ show e

-- Uncurry
app f [Pure a] = AppE f (atom a)
app f ((Pure a):as) = app (AppE f (atom a)) as
  





