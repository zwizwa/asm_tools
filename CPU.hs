-- A CPU?

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPU where
import Seq
import Control.Monad

-- How to begin?  Memory seems to be the most important component.
-- I'm going to target the iCE40, which has a bunch of individual
-- memories, allowing separate busses for instruction, data and return
-- stack, and the rest bundled as data memory.

-- At every clock, each of the 4 memories has a word sitting on its
-- read port:
-- i: current instruction
-- d: 2nd on stack  (top is in a register)
-- r: return address (ip is the instruction memory's write port)

-- The instruction word drives the decoder, which drives all the
-- muxes.

-- It seems that reading out instructions is the most useful thing to
-- start with.  This could be used for other kinds of sequencers that
-- are not necessarily general purpose CPUs.


-- Start with emulating the memory.  Below is the MyHDL model for the
-- iCE40.  I can't emulate this inside the Seq language, so start with
-- making an emulator extension.
--
-- from myhdl import *
--
-- def ram(write_clock, write_addr, write_data, write_enable,
--         read_clock,   read_addr,  read_data):
--     """ 
--     Random access memory (RAM) with single read 
--     and single write port
--     """
--     assert len(write_data) == len(read_data)
--     assert len(write_addr) == len(read_addr)
--     dw = len(write_data)
--     na = 2**len(write_addr)
--     memory = [Signal(intbv(0)[len(write_data):]) for _ in range(na)]
--   
--     @always(write_clock.posedge)
--     def rtlwr():
--         if write_enable:
--             memory[write_addr].next = write_data
--
--     @always(read_clock.posedge)
--     def rtlrd():
--         read_data.next = memory[read_addr]
--            
--     return instances()
