{-# LANGUAGE ScopedTypeVariables #-}

module Weave where
import Data.List
import Pru

shift_loops nb_pre pad loop_start loop1 loop2 = code where
  -- pull out nb_pre instructions into the preroll, rotating loop body.
  (pre1, rest) = splitAt nb_pre loop1
  loop1' = rest ++ pre1

  -- merge preroll with supplied padding
  preamble = merge [pre1, pad (length pre1)]

  -- merge rotated loop1 with loop2
  loop = merge [loop1', loop2]

  -- flatten
  code =
    sequence_ preamble >>
    label loop_start >>
    sequence_ loop
         
-- interleaving merge
merge = concat . transpose
  
