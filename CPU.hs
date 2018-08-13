-- A CPU?

-- It is the natural progression from Seq and Pru.
-- . implement CPU in Seq
-- . generalize assembler from Pru

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module CPU where
import Seq
import SeqLib
import Control.Monad
import Control.Applicative
import Data.Bits hiding (bit)

-- Some notes on how this got built.

-- . It starts with a desire to build a stack machine, followed by a
--   realization of lack of insight.  As with many leaps: where to
--   begin?  For many tasks, a full processor is overkill, so what is
--   the essence?

-- . A relization that instruction sequencing is the key element that
--   distinguishes a state machine from a processor.  This required
--   support for memories, so they got built into Seq in several
--   iterations.  Experiments indicated that emulation was really
--   slow, so SeqTH/SeqPrim got built on top of STUArray.

-- . First attempts at modularizing the design.  It is obvious that a
--   CPU can't be anything else but a monadic function from input to
--   output, but it was not at all clear what this I/O would be, or
--   how to practically decompose the submachines.

-- . A design emerged for a cont/wait/jmp machine coupled to an
--   instruction memory, parameterized by an instruction decoder.

-- . The realization that the input/output of the instruction decoder
--   is a bus that can be lifted out of the composition.  I.e. a CPU
--   consists of exposed bus operations, with closed instruction
--   sequencing and internal processor state.

-- . The realization that the implementation of the bus is something
--   that happens at the very top.  The processor is truly "embedded",
--   and the bus devices are truly "periphery", sitting beteen processor
--   (bus) and external I/O.

-- . The realization (from SwapForth), that a stack is better
--   implemented as registers, and that it is trivial.


-- Original notes on stack macchines:

-- Memory seems to be the most important component.  I'm going to
-- target the iCE40, which has a bunch of individual memories,
-- allowing separate busses for instruction, data and return stack,
-- and the rest bundled as data memory.

-- At every clock, each of the 4 memories has a word sitting on its
-- read port:
-- i: current instruction
-- d: 2nd on stack  (top is in a register)
-- r: return address (ip is the instruction memory's write port)

-- The instruction word drives the decoder, which drives all the
-- muxes.

-- It seems that reading out instructions is the most useful thing to
-- start with.  This could be used for specialized sequencers that are
-- not necessarily general purpose CPUs.  This can then be gradually
-- extended to more abstract operations.


-- The main problem for building a CPU is to properly decompose the
-- decoder.  I'm not sure how to do this exactly, so just start in an
-- ad-hoc way.

-- There is some arbitraryness here: a hierarchy is created in the
-- nesting of the "close" operations.  The guideline is to abstract
-- away a register as soon as possible, i.e. move it to the inner part
-- of the hierarchy.

-- At the very top, there is:
-- . instruction memory access:
--   . read:  program sequencing
--   . write: bootloader
-- . BUS I/O (i.e. containing GPIO)

-- Each hierarchy level is an adaptation.  closeIW will abstract the
-- inner decoder as an iw -> jump operation, and insert the necessary
-- logic to either just advance to the next instruction, or perform a
-- jump.

data Ins r  = Ins {
  insWord :: r S
  }

data Control r = Control {
  controlLoop :: r S,
  controlJmp  :: r S,
  controlArg  :: r S
  }

-- The interface to the outside consists of GPIO and iMem write access.
-- Maybe it is time to start parameterizing.

data IMemWrite r = IMemWrite {
  iMemWriteEn   :: r S,
  iMemWriteAddr :: r S,
  iMemWriteData :: r S
  }

-- Reads are closed, so sizes are necessarily taken from the size of
-- the write bus address and data registers.  If there is none
-- (e.g. if IMem is a ROM), then this can be used to specify size.
noIMemWrite :: Seq m r => Int -> Int -> IMemWrite r
noIMemWrite ibits abits = IMemWrite e w d where
  e = constant $ bit
  w = constant $ bits abits
  d = constant $ bits ibits


closeIMem :: Seq m r =>
  IMemWrite r
  -> r S
  -> (Ins r -> m (Control r, o))
  -> m o

closeIMem (IMemWrite wEn wAddr wData) run execute = do
  t_wAddr <- stype wAddr
  t_wData <- stype wData
  closeMem [t_wData] $ \[iw] -> do
    (ipNext, o) <- closeReg [t_wAddr] $ \[ip] -> do
      -- Execute instruction, which produces control flow information.
      iw' <- if' run iw 0
      (Control loop jump ipJump, o) <- execute (Ins iw')
      ipCont   <- inc ip
      rst      <- inv run
      [ipNext] <- cond
                  [(rst,  [0]),
                   (loop, [ip]),
                   (jump, [ipJump])]
                  [ipCont]
      "ip" .= ip
                  
      return ([ipNext], (ipNext, o))  -- comb ipNext to avoid extra delay
    return ([(wEn, wAddr, wData, ipNext)], o)

-- A simple test for closeIMem:
-- . program outputs iw as output
-- . tied to a memory writer defined in the test lib

-- The next thing should be to test this on hardware, but this
-- requires code initialization primitives.


-- The origianl problem that drove this exploration is meanwhile
-- implemented on PRU.  These were the instructions needed:
-- 
-- a) loop n times
-- b) write UART byte, wait until done
-- c) wait
-- d) set I/O
-- e) read I/O into memory and advance pointer

-- To implement loops, it would be useful to have a stack to be able
-- to have nested loop counters.  This would mean less registers.  I'm
-- not going to be able to make this simpler than making a small forth
-- machine..  This way:

-- UART out can be bit-banged.
-- Multiple counters not needed for timing control.
-- No "wait" instruction needed: instruction counting suffices.
-- Add a data stack when needed.  Probably a single top register is enough.

-- The basic instructions seem straightforward.  This is just a
-- decoder that fans out into mux controls.  The unknown part to me is
-- the call/return.

-- Call:   move IP+1 -> rtop write port
--         inc rpointer
--         set ip from instruction word
-- Ret:    dec rpointer
--         move rtop -> IP

-- This could also be microcoded:
-- a) load literal into rdata
-- b) increment rstack
-- c) unconditional jump

-- The operations that can be reused are:
-- write, postinc  (stacks + buffers)
-- read, predec

-- So there is a clear tradeoff between the complexity of the
-- instruction decoder, and the amount of instructions needed.

-- Where to start?  Conditional memory write.

-- So for unidirectional flow, this is easy.  For bi-directional such
-- as a stack, two pointers need to be maintained.  It might be
-- simplest to initialize them such that the write/read operation can
-- happen immediately?  Both will have individual adders.  Maybe not a
-- good idea?



-- The important realization is that what a CPU does is to manipulate
-- a bus.  The idea of a bus provides a decoupling point for
-- modularity.

-- I'm adopting a slight modification of the memory bus already used.
-- The addition is a busReadRdy signal, which allows reads to take
-- more than one cycle, and allows the cpu to wait for a particular
-- signal to appear.  Seperate read/write addresses are removed as the
-- CPU is kept simple: no read/write at the same time.

data BusIn r = BusIn {
  busReadRdy  :: r S,
  busReadData :: r S
}
data BusOut r = BusOut {
  busWriteEn   :: r S,
  busAddr      :: r S,
  busWriteData :: r S
}



-- Perform an operation and wait for it to finish.
-- Let's keep the operation abstract, so what this does is:
--
-- . first time the instruction is executed, the sub-machine is
--   enabled.  the sequencer will wait until the machine provides a
--   "done" flag, which will advance the instruction pointer.
--
-- . it seems simpler to split this into "start" and "wait"
--   instructions.
--


-- Some testing


-- Defaults for startup, memory size, and no memory write (rom).
cpu_imem :: Seq m r => Int -> Int -> (Ins r -> m (Control r, BusOut r)) -> m (BusOut r)
cpu_imem abits ibits decode = do
  -- Do not update the instruction pointer at the first instruction,
  -- as rData will be invalid.  The first cycle is used to perform the
  -- first instruction read.  After that, instruction pointer is updated.
  run <- seq01 -- 0,1,1,1....
  closeIMem (noIMemWrite ibits abits) run decode


data OpCode = IJMP

opcode :: OpCode -> Int
opcode IJMP = 0x80




-- This needs to grow, but it seems simplest to keep the encoding
-- abstract, so it can be optimized later on.  Do not rely on the
-- numeric values of these.
o_nb_bits  = 3

o_nop   = 0
o_jmp   = 1
o_push  = 2
o_drop  = 3
o_read  = 4
o_write = 5
o_swap  = 6
o_loop  = 7

i1 :: Int -> Int -> Int
i1 opc arg = opc `shiftL` (16 - o_nb_bits) .|. (arg .&. 0xFF)

i0 c = i1 c 0

-- These will be used directly to construct programs, so use simple names.
nop   = i0 o_nop
jmp   = i1 o_jmp
push  = i1 o_push
drop  = i0 o_drop
read  = i1 o_read
write = i1 o_write
swap  = i0 o_swap
loop  = i1 o_loop

-- Each instruction can have push/pop/write/nop wrt imm?  It seems
-- possible that stack can be manipulated in parallel with bus
-- transfer.

-- But let's not make this too complicated.  Some observations:

-- . This is for very low level, specialized code.  It will never
--   necessary to manipulate addresses as data, so address for read,
--   write can always come from the immediate word.  The data itself
--   might be manipulated.


-- It's probably ok to instantiate it fully even if certain
-- instructions are not used.  Yosys/abc removes unused logic.

-- Still, this can use some decomposition.  For now, because there are
-- not many instructions, use one-hot encoding to keep the logic
-- simple.

bus_master :: Seq m r => BusIn r -> m (BusOut r)
bus_master busIn = busOut where
  execute = stack_machine 3 busIn
  busOut = cpu_imem 8 16 execute

stack_machine ::
  Seq m r =>
  Int ->
  BusIn r ->
  Ins r ->
  m (Control r, BusOut r)

stack_machine  nb_stack (BusIn rReady rData) (Ins iw) = do
  -- Register file is organized as a stack
  let heads    = take (nb_stack - 1)
      tail2    = tail . tail
      t_stack  = replicate nb_stack $ bits 8
  
  closeReg t_stack $ \stack@(top:snd:_) -> do
    arg   <- slice' iw  8 0
    opc   <- slice' iw 16 (16 - o_nb_bits)
    
    let opc' n = equ opc $ cbits o_nb_bits n

    jmp   <- opc' o_jmp
    loop  <- opc' o_loop
    read  <- opc' o_read
    write <- opc' o_write
    push  <- opc' o_push
    drop  <- opc' o_drop
    swap  <- opc' o_swap

    dec'   <- dec =<< conc (cbit 0) top
    top'   <- slice' dec' 8 0
    carry  <- slice' dec' 9 8
    ncarry <- inv carry

    loop_jmp <- loop `band` ncarry
    loop_cnt <- loop `band` carry

    -- FIXME: drop is write to nowhere
    drop' <- (drop  `bor`) =<<
             (write `bor` loop_cnt)

    push_read <- read `band` rReady

    push' <- push `bor` push_read
    wait  <- (read `band`) =<< inv rReady
  
    jmp'  <- jmp `bor` loop_jmp

    new   <- if' push arg rData

    stack'@(top':snd':_) <- cond
      [(push', new  : (heads stack)),
       (drop', tail stack ++ [0]),
       (swap,  snd  : top : (tail2 stack)),
       (loop,  top' : tail stack)]
      stack

    -- reads can take multiple cycles.  it is assumed that rReady is
    -- high for only one cycle.

    -- probes
    "iw"  .= iw
    "top" .= top'
    "snd" .= snd'
    "c"   .= carry
    
    return (stack',                   -- internal reg feedback
            (Control wait jmp' arg,   -- instruction sequencer feedback
             BusOut write arg top))   -- top level output is a bus



-- Close over bus master (cpu) and bus slaves (peripherals) to create
-- a system on chip.

-- FIXME: make register addresses abstract.


soc :: Seq m r => [r S] -> m [r S]
soc [rx] = do
  
  closeReg [bit, bits 8] $ \[rStrobe,rData] -> do

    -- The effect of the CPU is a bus request.
    (BusOut wStrobe addr wData) <-
      bus_master (BusIn rStrobe rData)

    -- Instantiate peripherals, routing i/o registers.
    addr' <- slice' addr 2 0
    let tx_bc = 1 -- FIXME
        wOp n = addr' `equ` n >>= band wStrobe

    -- Bus write operations
    tx_wc <- wOp 2
    (tx, tx_rdy) <- async_transmit tx_bc (tx_wc, wData)

    -- Bus read operations
    busin <- switch addr'
      [(0,
        do
          (s,d) <- async_receive 8 rx;
          "rx_s" .= s
          "rx_d" .= d
          return [s,d]),
       (1,
        do
          -- Blocking read is convenient.
          -- Alternatively, implement this as a global flag.
          return [tx_rdy,0])]
      (return [0,0])
      
    return (busin, [tx])


soc_test :: Seq m r => [r S] -> m [r S]
soc_test ins = do
  outs <- soc ins
  -- Testing only uses named internam probe signals, not the
  -- production circuit's outputs.
  return []
