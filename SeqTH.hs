-- Template Haskell rendering of a Seq program to remove
-- interpretative overhead.

-- Since Template Haskell is still a little unfamilar to me, I'm not
-- writing this as a tagless final interpreter, but as an explicit
-- compiler using SeqTerm

{-# LANGUAGE TemplateHaskell #-}

module SeqTH where
import SeqTerm
import qualified Seq
import qualified SeqLib
import Language.Haskell.TH

x_seqTH = m where
  (outputs, bindings) = compile $ do
    t <- SeqLib.counter (Seq.SInt (Just 4) 0)
    return [t]
  m = do
    putStrLn "-- x_seqTH"
    print outputs
    sequence $ map print bindings
    expr <- runQ [| \f g x -> f (x*2 + 3) . g |]
    putStrLn $ pprint expr

    

