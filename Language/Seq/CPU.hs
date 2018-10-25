
-- A small control CPU.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Seq.CPU where
import Language.Seq
import Language.Seq.Lib
import qualified Language.Seq.Forth as Forth

import Control.Monad
import Control.Applicative
import Data.Bits hiding (bit)
import qualified Data.ByteString.Lazy as ByteString
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Get
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

-- Some realizations came up when this got built.

-- . It starts with a desire to build a stack machine, followed by a
--   realization of lack of insight.  As with many leaps: where to
--   begin?  For many tasks, a full processor is overkill, so what is
--   the essence?

-- . Instruction sequencing is the key element that distinguishes a
--   state machine from a processor.  This required support for
--   memories, so they got built into Seq in several iterations.
--   Experiments indicated that emulation was really slow, so
--   SeqTH/SeqPrim got built on top of STUArray.

-- . First attempts at modularizing the design.  It is obvious that a
--   Seq CPU can't be anything else but a monadic function from input
--   to output, but it was not at all clear what this I/O would be, or
--   how to practically decompose the submachines.

-- . A design emerged for a cont/wait/jmp machine coupled to an
--   instruction memory, parameterized by an instruction decoder.

-- . The essence of a bus.  The input/output of the instruction
--   decoder is a bus that can be lifted out of the composition.
--   I.e. a CPU consists of exposed bus operations, with closed
--   instruction sequencing and internal processor state.

-- . The implementation of the bus is something that happens at the
--   very top.  The processor is truly "embedded", and the bus devices
--   are truly "periphery", sitting beteen processor (bus) and
--   external I/O.

-- . (from SwapForth), A stack is better implemented as registers
--   instead of memory, and it is trivial.  Also, the resource use of
--   additional registers is minimal.  Thise can be added as code
--   needs it.

-- . A "control processor" (CP) doesn't need two stacks because lack
--   of data flow between instructions.  The itch being scratched is
--   the replacement of state machines with something more modular and
--   flexible.  What a CP needs is nested loops and procedure calls.

-- . A full Forth language is not necessary, because many control
--   structs can be implemented as meta functions in the Monad.
--




-- The sequencer and the instruction memory can be decoupled using
-- these interfaces:
data Ins r  = Ins {
  insWord :: r S,
  insPtr  :: r S
  }
data Control r = Control {
  controlLoop :: r S,
  controlJmp  :: r S,
  controlArg  :: r S
  }

-- The CPU can't write instruction memory.  The write port is
-- connected to the external world through SPI.
data MemWrite r = MemWrite {
  memWriteEn   :: r S,
  memWriteAddr :: r S,
  memWriteData :: r S
  }

-- That interface also determines sizes.  If there is no writer
-- (e.g. a ROM), the following stub can be used to specify size.
noMemWrite :: Seq m r => Int -> Int -> MemWrite r
noMemWrite ibits abits = MemWrite e w d where
  e = cbit 0
  w = cbits abits 0
  d = cbits ibits 0


-- The instruction sequencer fetches instructions from the instruction
-- memory and passes them to a custom 'execute' module, expecting back
-- a control instruction to describe what to do next.
sequencer :: Seq m r =>
  MemWrite r
  -> r S
  -> (Ins r -> m (Control r, o))
  -> m o
sequencer (MemWrite wEn wAddr wData) run execute = do
  t_wAddr <- stype wAddr
  t_wData <- stype wData
  closeMem [t_wData] $ \[iw] -> do
    (ipNext, o) <- closeReg [t_wAddr] $ \[ip] -> do
      iw'    <- if' run iw 0
      ipCont <- inc ip
      -- Execute instruction, which produces control flow information.
      (Control loop jump ipJump, o) <- execute (Ins iw' ipCont)

      rst      <- inv run
      [ipNext] <- cond
                  [(rst,  [0]),
                   (loop, [ip]),
                   (jump, [ipJump])]
                  [ipCont]

      "ip"  <-- ip
      "run" <-- run
                  
      return ([ipNext], (ipNext, o))  -- comb ipNext to avoid extra delay
    return ([(wEn, wAddr, wData, ipNext)], o)



-- The important realization is that what a CPU does is to manipulate
-- a bus.  The idea of a bus provides a decoupling point for
-- modularity.

-- I'm adopting a slight modification of the memory bus already used.
-- The addition is a busReadRdy signal, which allows reads to take
-- more than one cycle, and allows the cpu to wait for a particular
-- signal to appear.  Separate read/write addresses are removed as the
-- CPU is kept simple: no read/write at the same time.

data BusRd r = BusRd {
  busReadRdy  :: r S,
  busReadData :: r S
}
data BusWr r = BusWr {
  busReadEn    :: r S,  -- needed for side-effecting read, e.g. pop
  busWriteEn   :: r S,
  busAddr      :: r S,
  busWriteData :: r S
}



-- Instructions

-- This needs to grow, but it seems simplest to keep the encoding
-- abstract, with both the assembler and the instruction decoder using
-- the functions below.  The only reason not to change the numbers is
-- that a certain familiarity arises after some debugging.

o_nb_bits  = 4

o_nop   = 0
o_jmp   = 1
o_push  = 2
o_down  = 3
o_read  = 4
o_write = 5
o_swap  = 6
o_loop  = 7
o_call  = 8
o_ret   = 9
-- 10
-- 11
-- 12
-- 13
-- 14
-- 15


itable :: [(Int, (String, Int))]
itable =
  [(o_nop,   ("nop",   0)),
   (o_jmp,   ("jmp",   1)),
   (o_push,  ("push",  1)),
   (o_read,  ("read",  1)),
   (o_write, ("write", 1)),
   (o_swap,  ("swap",  0)),
   (o_loop,  ("loop",  1)),
   (o_call,  ("call",  1)),
   (o_ret,   ("ret",   0))]

dasm iw = "(" ++ hex ++ " " ++ asm ++ ")" where
  
  o = iw `shiftR` (16 - o_nb_bits)
  arg = iw .&. 0xFF
  Just (opc, narg) = lookup o itable
  asm =
    if o==o_write && arg==0 then
      "drop"
    else
      case narg of
        0 -> opc
        1 -> opc ++ " " ++ show arg

  hex' = showIntAtBase 16 intToDigit iw "" 
  hex  = replicate (4 - length hex') '0' ++ hex'

   

-- Use Forth for flow control structures.

-- Note that this is a Forth / Asm hybrid embedded in a Monad:
-- primitive operations do not have to be nullary.
ins i = Forth.save [i]

asm opc arg = opc `shiftL` (16 - o_nb_bits) .|. (arg .&. 0xFF)

i1 :: Int -> Int -> Forth.M ()
i1 opc arg = ins $ asm opc arg
i0 c = i1 c 0

nop   = i0 o_nop
jmp   = i1 o_jmp
push  = i1 o_push
read  = i1 o_read
write = i1 o_write
swap  = i0 o_swap
loop  = i1 o_loop
drop  = write drop_addr
call  = i1 o_call
ret   = i0 o_ret
down  = i0 o_down

-- In any case, a monad implementing a Forth compile stack is
-- convenient if only because of familiarity.
for   = Forth.mark
next  = Forth.cpop >>= loop
begin = Forth.mark
again = Forth.cpop >>= jmp


-- But with Haskell as meta-langauge, there is not a lot of need for
-- specific control structures beyond what is offered by the primitive
-- instructions.  E.g.:
for' n code = do push n ; for ; code ; next
forever m = begin >> m >> again


program = Forth.program $ asm o_jmp
fun     = Forth.fun     $ asm o_call
start   = Forth.fun'



-- For sending to a running device, pack programs as binary.

-- Put it in a representation that can be sent as bytes over SPI.
writeProgram :: String -> Forth.Program -> IO ()
writeProgram name lst = do
  ByteString.writeFile name $ packProgram lst

-- packProgram returns binary data, encoded such that it can be sent
-- over SPI using an 8-bit transfer, most significant bit sent
-- first. (D7-D0, the most common configuration and what is used by
-- iCE40 boot).  The SPI receiver is implemented as 16-bit, most
-- significant bit first.  This means the data inside the binary is
-- 16bit big endian.
  
packProgram = toLazyByteString . mconcat . (map (putWord16be . fromIntegral))

-- Undo packProgram to recover the word list.
unpackProgram :: ByteString.ByteString -> Forth.Program
unpackProgram bytes = prog where
  prog = runGet get bytes
  nb_words = (ByteString.length bytes) `div` 2 
  get = do
    ws <- sequence $ [getWord16be | _ <- [1..nb_words]]
    return $ map fromIntegral ws



-- Decode, Execute


-- The "read" and "write" directions are from the perspective of the
-- CPU (i.e. the programmer).

stack_machine ::
  Seq m r =>
  Int ->
  BusRd r ->
  Ins r ->
  m (Control r, BusWr r)

stack_machine  nb_stack (BusRd bus_rdy bus_data) (Ins iw ip) = do

  -- Word size is determined by pointer size
  word_size <- sbits ip

  -- Registers are organized in a stack
  let t_stack  = replicate nb_stack $ bits word_size
  
  closeReg t_stack $ \stack@(top:snd:_) -> do
    
    -- Decode instructions
    arg   <- slice' iw word_size 0
    opc   <- slice' iw 16 (16 - o_nb_bits)
    let opc' n = equ opc $ cbits o_nb_bits n
    
    call  <- opc' o_call
    jmp   <- opc' o_jmp
    ret   <- opc' o_ret
    write <- opc' o_write
    push  <- opc' o_push
    swap  <- opc' o_swap
    loop  <- opc' o_loop
    read  <- opc' o_read
    down  <- opc' o_down

    -- ALU operations
    (carry, top_dec) <- carry dec top
    ncarry           <- inv carry

    -- control flow
    loop_again <- loop .& ncarry
    loop_done  <- loop .& carry
    goto_en    <- one_of [ jmp, loop_again, call, ret ]
    goto_addr  <- if' ret top arg

    -- read instruction
    bus_nrdy  <- inv bus_rdy
    wait      <- read .& bus_nrdy
    push_read <- read .& bus_rdy

    -- stack operation flags
    pop  <- one_of [ write, loop_done, ret ]
    push <- one_of [ push, push_read, call ]
    
    -- what to push
    [push_data] <- cond
                   [(call,      [ip]), 
                    (push_read, [bus_data])]
                   [arg]

    set <- loop .| down
    let set_data = top_dec

        stac_ = take (nb_stack - 1) stack
        _tack = tail stack
        __ack = tail _tack
        

    stack'@(top':snd':_) <- cond
      [(push,   push_data : stac_),
       (pop,    _tack ++ [0]),
       (swap,   snd : top : __ack),
       (set,    set_data : _tack)]
      stack

    -- reads can take multiple cycles.  it is assumed that rReady is
    -- high for only one cycle.

    -- A note about probes: don't mix inputs and 'next' values, as
    -- this makes output very hard to read.

    -- probes
    "iw"  <-- iw
    "top" <-- top
    "snd" <-- snd
    "c"   <-- carry
    
    return (stack',                           -- internal reg feedback
            (Control wait goto_en goto_addr,  -- instruction sequencer feedback
             BusWr read write arg top))       -- top level output is a bus



-- Close over bus master (cpu) and bus slaves (peripherals) to create
-- a system on chip.

-- Register addresses are abstract similar to instruction encodings.

drop_addr    = 0 :: Int
uart_rx_addr = 1 :: Int
uart_tx_addr = 2 :: Int
dbg_addr     = 3 :: Int
-- -1 is the void used for "drop"

bus [rx, tx_bc] (BusWr rEn wEn addr wData) = do
  
  -- Instantiate peripherals, routing i/o registers.
  addr' <- slice' addr 2 0
  let c = cbits 2  -- numeric case used in switch
      wEn' n = (addr' .== c n) >>= band wEn  -- register write clock

  -- Bus write operations

  -- UART.  Baud generator should probably be a circuit.
  tx_wc <- wEn' uart_tx_addr
  tx_data <- slice' wData 8 0
  (tx, tx_rdy) <- async_transmit tx_bc (tx_wc, tx_data)

  -- Debug writes.  Used for
  -- 1) CPU test programs in test suite
  -- 2) FPGA LED output (see with_debug and f_sock.hs)
  dbg_wc <- wEn' dbg_addr
  "bus_data" <-- wData
  "bus_dbg"  <-- dbg_wc

  -- Bus read operations.  It's ok to leave these always on, e.g. for
  -- streaming data ports, but when a read causes a side effect, the
  -- rEn bit should be used.
  [rStrobe, rData] <- switch addr'
    [(c uart_rx_addr,
      do
        (s,d) <- async_receive 8 rx;
        "rx_s" <-- s
        "rx_d" <-- d
        return [s,d]),
     (c uart_tx_addr,
      do
        -- Blocking read is convenient.
        -- Alternatively, implement this as a global flag.
        return [tx_rdy,0])]
    (return [0,0])
  return (BusRd rStrobe rData, [tx])

-- For now these are fixed to 16 bit instruction words
spi_boot nb_bits cs sck sda = do
  (wc, w) <- sync_receive Mode3 16 cs sck sda
  a <- arrPtr (bits nb_bits) cs wc
  return $ MemWrite wc a w

-- rom_boot :: Seq m r => IMemWrite r
-- rom_boot = noIMemWrite 16 8

soc :: Seq m r => [r S] -> m [r S]
soc [rx,tx_bc,cs,sck,sda] = do

  -- FIXME: create a more permissive bus structure that allows
  -- stubbing out some signals.
  

  -- A soc is the bundling of a CPU, an instruction memory, sequencer,
  -- boot circuit, a peripheral bus, and a reset circuit.
  let
    -- Instruction width is fixed at 16 bits for now
    ins_bits  = 16

    -- Memory address size.  Also sets stack and bus width.  Note that
    -- this has a large effect on the circuit size.
    imem_bits = 12

  -- Do not update the instruction pointer at the first instruction,
  -- as rData will be invalid.  The first cycle is used to perform the
  -- first instruction read.  After that, instruction pointer is updated.
  mem_wait <- seq01 -- 0,1,1,1....

  -- Processor can be restarted with a program from SPI.
  nrst <- mem_wait .& cs
  boot <- spi_boot imem_bits cs sck sda
  
  let 
    -- Couple CPU and instruction memory
    bus_master busIn = busOut where
      execute = stack_machine 4 busIn
      busOut = sequencer boot nrst execute
  
  -- Couple bus master and slave through bus registers.
  closeReg [bit, bits imem_bits] $ \[rStrobe,rData] -> do
    bus_wr <- bus_master (BusRd rStrobe rData)
    (BusRd rStrobe' rData', soc_output) <- bus [rx, tx_bc] bus_wr
    return ([rStrobe', rData'], soc_output)


soc_test :: Seq m r => [r S] -> m [r S]
soc_test ins = do
  outs <- soc ins
  -- Testing only uses named internal probe signals, not the
  -- production circuit's outputs.
  return []
