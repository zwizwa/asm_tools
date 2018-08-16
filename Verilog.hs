-- Since we're just generating RTL, there is a fairly direct mapping
-- from SeqTerm to Verilog.

module Verilog where

import Seq
import SeqTerm


-- Some simplifications:
-- . All signals are vectors.
-- . Keep operations in ANF
-- . One block for registers, CLK, RST
-- . One block for memories, CLK only


sig kind (SInt (Just n) _) name = kind ++ " [" ++ show (n-1) ++ ":0] " ++ name ++ ";"
wire   = sig "wire"
reg    = sig "reg"
input  = sig "input"
output = sig "output"

