{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}

module Language.Seq.Lib where
import Language.Seq
import Control.Monad
import Control.Applicative
import Data.Key(Zip(..),zipWith, zip)
import Prelude hiding (zipWith, zip)
import Data.Bits hiding (bit)

-- Some general guidelines.
--
-- (1) As a general rule, don't write library routines that take monadic
--     values as arguments, unless they behave as macros and instantiate
--     conditionally or multiple times.
--
--     The exception seems to be switch.  The reason?  It allows the use
--     of local variables in the clauses to make the code more readable.
--     Even if the signals could be reused, they likely won't be,
--     because the behavior "belongs" to that clause.
--
-- (2) It seems more appropriate to write in ANF form.  Do notation is
--     really awkward for an expression language.  (Note: the reasons
--     for the Monad are sharing and metaprogramming).  There is a
--     readability tradeoff:
--
--     - imperative update
--
--     - lots of names for different intermediates
--
--     - lots of connective glue noise, e.g. >>=, lift2
--
--     I've come to the conclusion that imperative form is better.  To
--     solve this issue, create a syntactic overlay or use Conal
--     Eliott's 'concat' work.
--
-- (3) Since Seq is RTL with an abstract clock, the word "clock" can
--     be repurposed to mean "clock enable".  The latter is too
--     cumbersome.  The notational convention is to postfix
--     identifiers with "c".  E.g. "wc" is word clock (enable), "bc"
--     is bit clock (enable).
--
--     Note that when dealing with external signals such as SPI, there
--     will be a "real" clock signal.  The idea there is to sample it
--     and convert it to a clock enable signal as soon as possible,
--     and not carry around sampled external clocks.





-- Convert a register update equation to to monadic output value,
-- "tucking away" the registers.  This effectively computes the fixed
-- point with register decoupling, defining a sequence.

-- The idea here is that using 'signal' and 'update' in the same
-- function will avoid the creation of undriven or multiply-driven
-- signals.  This is preferred over using the low-level primitives
-- directly.

-- A meta-level container is used to bundle registers.  Typically, a
-- List will do, but the interface is generic.  Note: liftA2 doesn't
-- do the right thing on lists.

-- This is the general form, parameterized by a generic next operator.

closeReg' ::
  forall f m r o. (Zip f, Traversable f, Seq m r) =>
  (r S -> r S -> m ())
  -> f SType
  -> (f (r S) -> m (f (r S), o))
  -> m o
closeReg' update' ts f = do
  rs <- sequence $ fmap signal ts
  (rs', o) <- f rs
  sequence_ $ zipWith update' rs rs'
  return o


-- The default closeReg uses a "update" that is parameterized by the
-- enable bit for the local context.  This is useful for building
-- state machines that are updated at a lower rate.

closeReg ::
  forall f m r o. (Zip f, Traversable f, Seq m r) =>
  f SType
  -> (f (r S) -> m (f (r S), o))
  -> m o
closeReg = closeReg' updateEnable

closeRegEn ::
  forall f m r o. (Zip f, Traversable f, Seq m r) =>
  r S
  -> f SType
  -> (f (r S) -> m (f (r S), o))
  -> m o
closeRegEn en = closeReg' $ updateIf en

updateEnable :: forall m r. Seq m r => r S -> r S -> m ()
updateEnable r v = do
  env <- getEnv
  case env ClockEnable of
    Nothing ->
      update r v
    Just en' ->
      updateIf en' r v

-- Note that when using clock enables, it is important to only ever
-- use the outputs of the register, and not the input combinatorial
-- networks!
withClockEnable val m = do
  let f _ ClockEnable = Just val
      f env var = env var
  withEnv f m

updateIf :: forall m r. Seq m r => r S -> r S -> r S -> m ()
updateIf en r v = do
  v' <- if' en v r
  update r v'
  


-- Similar, but for memories
closeMem :: 
  forall m r f o. (Seq m r, Zip f, Traversable f) =>
  f SType -> (f (r S) -> m (f (r S, r S, r S, r S), o)) -> m o

closeMem typ memAccess = do
  mems <- sequence $ fmap memory typ
  let rds     = fmap fst mems
      memRefs = fmap snd mems
  (memInputs, o) <- memAccess rds
  sequence $ zipWith updateMemory memRefs memInputs
  return o




-- Tese are different from fmap, liftA2 because the primitive
-- operations are monadic.
lift1 f ma       = do a <- ma                     ; f a
lift2 f ma mb    = do a <- ma ; b <- mb           ; f a b
lift3 f ma mb mc = do a <- ma ; b <- mb ; c <- mc ; f a b c



-- Shortcuts for common type constructions.

sbits' :: Seq m r => r S -> m (Maybe Int)
sbits' sig = do
  (SInt sz _) <- stype sig
  return sz

sbits :: Seq m r => r S -> m Int
sbits sig = do
  t <- sbits' sig
  case t of
    (Just n) -> return n
    _ -> error $ "sbits: undefined bit size"


-- Bit size and reset value
bits' :: Int -> Int -> SType
bits' n = SInt (Just n)
bit' = bits' 1

-- Bit size only, reset value at 0
bits :: Int -> SType
bits n = bits' n 0
bit = bits 1

-- Fixed size constants.
cbit :: Seq m r => Int -> r S
cbit = constant . bit'

cbits :: Seq m r => Int -> Int -> r S
cbits n = constant . (bits' n)

-- Special closeReg case: single register, with register as output
reg1 :: Seq m r => SType -> (r S -> m (r S)) -> m (r S)
reg1 t f = do closeReg [t] $ \[r] -> do r' <- f r ; return ([r'], r)

-- Register with write enable.  This is what a programmer thinks upon
-- hearing "register": something that can be written to.
register we v = do
  t <- stype v
  withClockEnable we $ reg1 t $ \_ -> return v







-- Some simple building blocks

inc :: Seq m r => r S -> m (r S)
inc c = add c 1

top_bit v = do
  n <- sbits v
  slice' v n (n-1)
bottom_bits v = do
  n <- sbits v
  slice' v (n-1) 0

carry :: Seq m r => (r S -> m (r S)) -> r S -> m (r S, r S)
carry inc c = do
  inc' <- inc =<< conc (cbit 0) c
  c    <- top_bit inc'
  bits <- bottom_bits inc'
  return (c, bits)


dec :: Seq m r => r S -> m (r S)
dec c = sub c 1

counter :: Seq m r => SType -> m (r S)
counter t = reg1 t inc




--- mod_*_counter: downcounters, using one extra bit as carry to avoid
--- comparator.



-- Free running arbitrary period downcounter,
-- combinatorial output for carry.
mod_counter' :: Seq m r => Int -> m (r S, r S)
mod_counter' period = do
  let n = nb_bits period
      init = cbits n $ period - 1
  closeReg [bits n] $ \[s] -> do
    (c, dec') <- carry dec s
    s'        <- if' c init dec'
    return ([s'], (c, s))

-- Registered output.
mod_counter :: Seq m r => Int -> m (r S, r S)
mod_counter period = do
  (c, cnt) <- mod_counter' period
  c' <- delay c
  return (c', cnt)

-- Free running 2^N upcounter, combinatorial output for carry flag
carry_counter' :: Seq m r => SType -> m (r S, r S)
carry_counter' t = do
  closeReg [t] $ \[s] -> do
    (c, s') <- carry inc s
    return ([s'], (c, s))

-- Registered output.
carry_counter t = do
  (c, cnt) <- carry_counter t
  c' <- delay c
  return (c', cnt)








delay :: Seq m r => r S -> m (r S)
delay x = do
  t <- stype x
  reg1 t $ \_ -> return x

edge d = do
  d0 <- delay d
  d `bxor` d0

edges d = do
  e <- edge d
  pos <- e `band` d
  neg <- (e `band`) =<< inv d
  return (pos, neg)

posedge d = fmap fst $ edges d
negedge d = fmap snd $ edges d


-- Synchronous S/R FF. Combinatorial output. The hard part here is to
-- determine what to do when both signals are active at the same time.
-- We pick reset as the one that overrides.
set_reset set reset = do
  closeReg [bit] $ \[s] -> do
    [s'] <- cond
            [(reset, [0]),
             (set,   [1])]
            [s]
    return ([s'], s')
  


-- Clock recovery is carry counter with half-bit reset.




-- SPI

-- Convention used in the Linux kernel, user space interface spi/spidev.h
--
-- #define SPI_CPHA		0x01
-- #define SPI_CPOL		0x02

-- #define SPI_MODE_0		(0|0)
-- #define SPI_MODE_1		(0|SPI_CPHA)
-- #define SPI_MODE_2		(SPI_CPOL|0)
-- #define SPI_MODE_3		(SPI_CPOL|SPI_CPHA)

-- Documentation/spi/spidev
-- SPI_CPOL: clock polarity, idle high iff this is set
-- SPI_CPHA: clock phase, sample on trailing edge iff this is set


-- Using the mode number seems most convenience.  Encode it as a flat
-- datatype, then define projections.

data SpiMode = Mode0 | Mode1 | Mode2 | Mode3 deriving Show
spi_mode Mode0 = (0,0)  -- sample 0->1
spi_mode Mode1 = (0,1)  -- sample 1->0
spi_mode Mode2 = (1,0)  -- sample 1->0
spi_mode Mode3 = (1,1)  -- sample 0->1

-- Derive bit clock from SPI signals and compile time mode parameter.
-- Combinatorial output to keep it aligned to corresponding data line.



spi_bc mode cs sck = do
  let (cpol, cpha) = spi_mode mode
      sample_edge = 1 `xor` cpol `xor` cpha
  
  closeReg [bit' cpol] $ \[d_sck] -> do
    ncs  <- inv cs
    edge <- sck .^ d_sck
    -- bc selects the correct edge when cs is active
    bc   <- sck .== cbit sample_edge
    bc   <- bc .& edge
    bc   <- bc .& ncs
    -- properly initialize the edge detector to the idle phase when
    -- not selected (cs=1, assuming active low)
    sck' <- if' cs (cbit cpol) sck
    return ([sck'], bc)





-- From the perspective of a decoder, it makes most sense to think of
-- initial phase and sampling edge.

-- mode cpol cpha edge
-- 0    0    0    0->1
-- 1    0    1    1->0
-- 2    1    0    1->0
-- 3    1    1    0->1


-- As an example, use the mode that is used by iceprog.c FT2232H MPSSE
-- mode configured using:
-- 
-- #define MPSSE_WRITE_NEG   0x01 /* Write TDI/DO on negative TCK/SK edge*/
--
-- This implies:
-- . the idle clock is 1
-- . data is written out at 1->0
-- . data should be read in at 0->1 (or using a fixed delay after 1->0)
--
-- So this is CPOL=1, CPHA=1  (mode 3),  samples on 0->1
--
-- The iCE40 supports both mode 0 and mode 3.  Both sample on 0->1.



-- At the read end, we use synchronous machines:
-- . Sample sck and sda using a double-D synchronizer
-- . Convert (cs,sck,sda) to a (rst,bc,b) signal

-- As a convention, rst=cs.

-- First, implement mode3 correctly.



-- For the read end, one would think that the only thing important is
-- sampling edges.  I.e. that the initial level of the clock doesn't
-- matter.
--
-- But when using an edge detector to find the edges, it is important
-- to initialize it properly such that there is no spurious pulse in
-- the beginning.
--
-- E.g. suppose initial clock phase is 1, but we initialize the edge
-- detector with 0.  If the sampling edge is 0->1, it will detect a
-- pulse when enabled.


-- What I want is a circuit that can sample on 0->1, but will work
-- with both inital phases.

-- To solve this, I see a couple of solutions:

-- .  Use a free-running edge detector and gate the output with the
--    chip select line.  This assumes that there is enough space
--    between the master setting the initial clock level and issuing
--    the chip select pulse.
--
-- .  Sample the value of the clock when CS goes low.
--
-- .  Explicitly initialize the delay
--








-- But the initial phase is important to properly reset the edge
-- detector in order not to introduce spurious pulses.  E.g. suppose
-- we're sampling on 0->1, but the initial clock phase is 1 and the
-- reset value of the delay is 0.  The circuit will see a spurious
-- pulse when enabled.

-- What does matter, is how the edge detector gets initialized, to
-- ensure there is no spurios pulse.  E.g. if the delay element is
-- initialized at 0, but the clock level starts out at 1, it will see
-- a pulse.



-- 1,1,1 and delayed versions
seq1 :: Seq m r => m (r S)
seq1 = closeReg [bit' 0] $ \[r] -> return ([1], r)

seq01 :: Seq m r => m (r S)
seq01 = delay =<< seq1


-- A test of completeness is to implement a clock synchronizer.
-- Simplify it to power-of-two division.

-- Combinatorial part
sync' :: forall m r. Seq m r => r S -> r S -> r S -> m (r S)
sync' s0 i s = do
  e  <- edge i   -- edge detector on input
  s' <- inc s    -- default is free running counter
  if' e s0 s'    -- conditional reset on edge
  
-- Bound to register
sync :: Seq m r => SType -> r S -> m (r S)
sync t i = do
  reg1 t $ sync' (constant t) i



-- Synchronizer for non 2^N periods, with framing.
sync_mod :: Seq m r => Int -> r S -> r S -> m (r S)
sync_mod period frame idata = do
  edge'  <- edge idata

  -- Hold reset between frame enabled and first edge.  This is
  -- represented as an adjusted framing signal: frame'
  start  <- edge' .& frame
  stop   <- negedge frame
  frame' <- set_reset start stop
  
  let n        = nb_bits period
      full_bit = cbits n $ period - 1
      half_bit = cbits n $ (period `div` 2) - 1
  closeReg [bits n] $ \[cnt] -> do
    (carry', dec') <- carry dec cnt

    -- Reset sub-bit clock to 1/2 bits on data edges, or when not
    -- enabled (= outside of frame).
    clk_rst <- (edge' `bor`) =<< inv frame'
    
    [cnt'] <- cond
      [(clk_rst, [half_bit]),
       (carry',  [full_bit])]
      [dec']

    sync_out <- carry' `band` frame'
    return ([cnt'], sync_out)


  


-- Shift register in terms of slice + conc.
shiftReg :: Seq m r => ShiftDir -> SType -> r S -> m (r S)
shiftReg dir tr i = do
  closeReg [tr] $ \[r] -> do
    r_shift <- shiftUpdate dir r i
    return $ ([r_shift], r)

data ShiftDir = ShiftLeft | ShiftRight


slice' :: Seq m r => r S -> Int -> Int -> m (r S)
slice' s hi lo = slice s (Just hi) lo


-- Inner routine is useful without feedback.
-- This is "natural order", or MSB first.
shiftUpdate dir r i = do
  r_bits <- sbits r
  i_bits <- sbits i
  case dir of
    ShiftLeft -> do
      r_drop <- slice r (Just $ r_bits - i_bits) 0
      conc r_drop i
    ShiftRight -> do
      r_drop  <- slice r (Just r_bits) i_bits
      conc i r_drop


integral :: Seq m r => r S -> m (r S)
integral x = do
  t <- stype x
  reg1 t $ add x


-- Multi-argument versions
sum :: Seq m r => [r S] -> m (r S)
sum = reduce ADD

reduce :: Seq m r => Op2 -> [r S] -> m (r S)
reduce ADD [] = return 0
reduce MUL [] = return 1
reduce XOR [] = return 0
reduce AND [] = return $ -1
reduce OR  [] = return 0
reduce opc [] = error $ "reduce: no identity element for " ++ show opc
reduce opc [a] = return a
reduce opc [a,b]  = (op2 opc) a b
reduce opc (a:as) = (reduce opc as) >>= (op2 opc) a










-- Note that Seq does not support "imperative" conditionals, which are
-- used in MyHDL to conditionally assign multiple signals.  To
-- implement this kind of behavior, use a Traversable Zip container
-- (e.g. []).  This make the parallel muxing behavior explicit.

ifs ::
  (Seq m r, Traversable f, Zip f) =>
  r S -> f (r S) -> f (r S) -> m (f (r S))
ifs c t f = sequence $ zipWith (if' c) t f

-- This makes me think that case statements might be implemented as
-- multiplexers as well?  does this need special care, or is it easy
-- for the synthesizer to recover the structure?  e.g. MyHDL does
-- perform some magic for "case" statements I believe.


-- Nested ifs
cond ::
  (Seq m r, Traversable f, Zip f)
  => [(r S, f (r S))]
  -> f (r S)
  -> m (f (r S))
cond [] dflt = return dflt
cond ((flag, whenTrue) : clauses) dflt = do
  whenFalse <- cond clauses dflt
  ifs flag whenTrue whenFalse


-- Convenience routine for switch statement.  The different branches
-- are left as monadic, which allows use of local bindings to make
-- code more readable.

-- Note: This likely needs to be re-grouped from flat SeqTerm form,
-- for MyHDL to generate a vhdl/verilog case statement.
switch ::
  (Seq m r, Traversable f, Zip f)
  => r S
  -> [(r S, m (f (r S)))]
  -> m (f (r S))
  -> m (f (r S))
switch state clauses dflt = do
  clauses' <-
    sequence $
    [do flag <- state `equ` key
        val  <- mval
        return (flag, val)
    | (key, mval) <- clauses]
  dflt' <- dflt
  cond clauses' dflt'


-- Determine bitlength necessary to represent a number.

nb_bits i = f 0 where
  f n = case shiftL 1 n > i of
    True -> n
    False -> f $ n + 1
    
t_nb_bits (SInt (Just n_bits) _) =  SInt (Just $ nb_bits n_bits) 0
t_nb_bits _ = SInt Nothing 0  


-- Streams

-- RTL is associated to streams by collecting the output of registers,
-- one for each clock cycle.

-- In practice, this is too limiting.  It is necessary to represent
-- streams at arbitrary rate.  This can be done by accompanying a
-- stream at clock rate, with a stream of enable bits, and using the
-- enable bits as a "synchronous clock".

-- For now it is ok to use partially applied pairs for this, as it
-- gives the necessary instances.  Maybe call these "tagged streams" ?
type Stream r = (,) r S




-- UART.
--
-- Some observations:
-- a) Factored into timing generation + shift register
-- b) Timing can be factored using an enable signal

-- The part below only computes timing. 

-- Detect start bit, go to "on" state and produce a number of pulses.
-- wait for stop bit and produce parallel output strobe.

-- ( Personal note: One of the hardest things to unlearn going from
-- CPU programming to hardware programming is that conditionals do not
-- "save work".  For this an instruction sequencer with conditional
-- branches is needed.  Conditionals in HDL are static multiplexers. )


-- If only a single UART is needed, it makes sense to associate a
-- single baud rate counter.  If many are needed, it's possible to
-- save some register bits by deriving from a shared clock.  How many
-- bits if internal resolution are needed in that case?  Assume the
-- rate is accurate enough.  What about the phase?


-- Debug version.  Outputs internal state.
async_receive_sample ::
  forall m r. Seq m r =>
  Int -> r S -> m (r S, r S)
async_receive_sample nb_data_bits@8 rx = sm where

  -- This could be parameterized, but really, I only need one kind.
  [idle, start, word, stop] = [0,1,2,3] :: [r S]

  half_bit =  4-1 :: r S
  one_bit  =  8-1 :: r S
  all_bits = 64-1 :: r S

  t_state = bits 2  -- state
  t_count = bits 6  -- sub-bit counter

  sm :: m (r S, r S)
  sm = closeReg [t_state,t_count] update

  update :: [r S] -> m ([r S], (r S, r S))
  update [state, count] = do
    -- Subcircuits hoisted out of conditional.
    (c,count1) <- carry dec count
    phase      <- slice' count 3 0
    bitClock   <- phase .== 0
    
    (bc:wc:regs') <- switch state [
      (idle,  do
          state' <- if' rx idle start
          -- skip half a bit to sample mid-bit
          return [0, 0, state', half_bit]),
      (start, do
          -- skip start bit
          [state', count'] <- ifs c [word, all_bits] [state, count1]
          return [0, 0, state', count']),
      (word,  do
          -- send out bit clock mid-bit
          [state', count'] <- ifs c [stop, one_bit] [state, count1]
          return [bitClock, 0, state', count']),
      (stop,  do
          -- only send out the word clock if stop bit is 1
          wordClock <- bitClock .& rx
          [state', count'] <- ifs c [idle, 0] [state, count1]
          return [0, wordClock, state', count'])]
      -- FIXME: not reached.  How to express mutex states?
      (return [0, 0, 0, 0])

    -- test probes
    "rx_in" <-- rx
    "rx_bc" <-- bc
    "rx_wc" <-- wc
      
    return (regs', (bc,wc))



async_receive nb_bits i = do
  (bc,wc) <- async_receive_sample nb_bits i
  sr <- withClockEnable bc $ shiftReg ShiftRight (bits nb_bits) i
  -- return (sr:regs)
  return (wc,sr)

-- FIXME: This is not very good.  Think in terms of specification:
-- . bitClock: clock out the next bit.  if stop bit, raise done
-- . wordClock: load shift register, regardless of bit clock

-- . it is allowed to clock in a new word during the stop bit.  to
--   make this work, the output should not change.



async_transmit bitClock (wordClock, txData) = do
  n <- sbits txData
  let n' = nb_bits $ n + 2
  closeReg [SInt (Just $ n+2) (-1),
            SInt (Just $ n') 0] $
    \[shiftReg, cnt] -> do


      -- Outputs are a function of previous state.
      out  <- slice' shiftReg 1 0
      done <- cnt `equ` 0

      -- Make it so that inserting a new word in the shift register
      -- during the stop bit doesn't change the bit output (== 1).
      -- LSB is stop bit, the next one is start bit of next word.
      -- Once fully shifted, contents is all 1, so also stop bit.
      newframe <- conc txData =<< conc (cbit 0) (cbit 1)
      shifted  <- conc (cbit 1) =<< slice' shiftReg (n+2) 1
      cntDec   <- if' done 0 =<< dec cnt

      [shiftReg', cnt'] <- cond
        [(wordClock, [newframe, cbits n' (n + 2)]),
         (bitClock,  [shifted,  cntDec])]
        [shiftReg, cnt]

      "tx_bc"   <-- bitClock
      "tx_wc"   <-- wordClock
      "tx_in"   <-- txData
      "tx_done" <-- done
      "tx_out"  <-- out
      "tx_sr"   <-- shiftReg
      
      -- return ([shiftReg', cnt'],
      --         [out, done,
      --          wordClock, bitClock, shiftReg', cnt'])

      return ([shiftReg', cnt'], (out, done))



-- SPI

-- It's assumed that signals are properly double-D synchronized.  Note
-- that there might be a delay difference between clock and data, so 

-- Sample on rising edge only.  I beleive this is the same as the iCE40.

-- Assume wordsize is power of two so we can roll around the counter.
-- FIXME: It's easier to reuse the counter and transfer directly into
-- the memory.
sync_receive mode nb_bits cs sclk sdata = do
  bc <- spi_bc mode cs sclk
  -- FIXME: shift register needs reset
  out@(wc, w) <- deser ShiftLeft nb_bits cs bc sdata
  "sclk"  <-- sclk
  "sdata" <-- sdata
  "s_bc"  <-- bc
  "s_wc"  <-- wc
  "s_w"   <-- w
  return out


-- Seserializer for converting bit streams to word streams.
-- Combinatorial output to allow for bc, wc to coincide.
-- The 'rst' input is an enable line, active low.

deser :: Seq m r => ShiftDir -> Int -> r S -> r S -> r S -> m (r S, r S)
deser dir sr_bits rst bc b = do
  -- Use carry trick to obtain word clock. The trick is then to
  -- properly initialize the counter.
  let cnt_bits  = nb_bits cnt_init'
      cnt_init' = sr_bits - 1
      cnt_init  = cbits cnt_bits $ cnt_init'
  closeReg [bits' cnt_bits cnt_init',
            bits sr_bits] $
    \[cnt, sr] -> do
      
      sr_shift     <- shiftUpdate dir sr b
      (c, cnt_dec) <- carry dec cnt
      
      [wc, cnt', sr'] <- cond
        [(rst, [0, cnt_init, 0]),
         (bc,  [c, cnt_dec,  sr_shift])]
        [0, cnt, sr]

      return ([cnt',sr'], (wc, sr'))





-- FIFO
-- Note: single cycle read-after-write hazard

fifo ta (rc,wc,wd) = do
  -- t: type
  -- d: data
  -- a: address
  -- c: clock enable
  td <- stype wd
  wa <- fifoPtr ta wc
  ra <- fifoPtr ta rc
  closeMem [td] $ \[rd] -> do
    return ([(wc, wa, wd, ra)], rd)


fifoPtr :: Seq m r => SType -> r S -> m (r S)
fifoPtr t wc = do
  closeReg [t] $ \[a] -> do
    a1 <- inc a
    a' <- if' wc a1 a
    return ([a'], a)

-- Polarity as used with SPI
arrPtr :: Seq m r => SType -> r S -> r S -> m (r S)
arrPtr t cs wc = do
  closeReg [t] $ \[a] -> do
    a1 <- inc a
    a' <- if' wc a1 a
    a' <- if' cs 0 a'
    return ([a'], a)



-- Stack / LIFO
-- Note: single cycle read-after-write hazard

-- 1. push A
-- 2. use rData == old value
-- 3. use rData == A

-- en:     write enable
-- upDown: 1:inc, 0:dec if enabled
-- out:    top



stack :: Seq m r => SType -> r S -> r S -> r S -> m (r S)
stack t_a en upDown wData = do
  t_d <- stype wData
  closeReg [t_a] $ \[ptr] -> do
    closeMem [t_d] $ \[rData] -> do
      ptrUpDown <- lift2 (if' upDown) (inc ptr) (dec ptr)
      ptrNext   <- if' en ptrUpDown ptr
      return ([(en, ptrNext, wData, ptrNext)],
               ([ptrNext], rData))


-- Variant: allow write without inc/dec
-- More suitable for stack CPU

-- 00 idle
-- 01 predec
-- 10 preinc
-- 11 update

stackUpDown :: Seq m r => SType -> r S -> r S -> r S -> m (r S)
stackUpDown t_a up down wData = do
  t_d <- stype wData
  closeReg [t_a] $ \[ptr] -> do
    closeMem [t_d] $ \[rData] -> do
      en        <- up `bor`  down
      move      <- up `bxor` down
      ptrUpDown <- lift2 (if' up) (inc ptr) (dec ptr)
      ptrNext   <- if' move ptrUpDown ptr
      return ([(en, ptrNext, wData, ptrNext)],
               ([ptrNext], rData))

-- FIXME: make it complementary preinc/postdec, whichever is easiest


-- Stack as a shift register?  See swapforth
-- https://github.com/jamesbowman/swapforth/blob/master/j1a/verilog/stack2.v


-- Select from list of signals.
index :: Seq m r => (r S) -> [r S] -> m (r S)
index sel sigs = index' sel zeroExtend where
  index' sel sigs = do
    mbits <- sbits' sel
    let bits = case mbits of
          (Just bits) -> bits
          Nothing -> error $ "index unrolls. needs fixed bit width"
    case bits of
      0 -> do
        return $ head sigs
      _ -> do
        sel'  <- slice' sel bits 1
        c     <- slice' sel 1    0
        even' <- index' sel' $ even sigs
        odd'  <- index' sel' $ odd  sigs
        if' c even' odd'
  zeroExtend = sigs ++ cycle [constant $ SInt Nothing 0] 
  even (a:_:as) = (a : even as)
  odd (a:as) = even as





-- Probes consist of two mechanisms:
--
-- . Meta-level, using the 'probe' primitive.  This records a binding
--   between the string name and the signal representation.  This is
--   used e.g. in renaming output signals for MyHDL output.
--
-- . Object level, where signals can be "teleported" into an enclosing
--   dynamic scope.  This is useful for defining debugging context.
--   E.g. on an FPGA board, LEDs could be assigned a name such that
--   they can be driven by deeper signals without a need for
--   propagation through the abstract interface.  ( Debugging is at
--   odds with data hiding. )



-- Convenient shortcut for test probe
(<--) :: Seq m r => String -> r S -> m ()
(<--) name val = do
  probe name val
  updateProbe name val
  

-- An 'update' variant that works on environment variables.

-- An attempt to create a different probe mechanism for FPGA circuits.
-- This is for debug only as it imposes global constraints.

withProbe str reg m = do
  let f env var@(Probe str') =
        case str == str' of
          True  -> Just reg
          False -> env var
      f env var = env var
  withEnv f m

updateProbe str val = do
  env <- getEnv
  case env (Probe str) of
    Nothing  -> return ()
    Just reg -> update reg val


-- These are just too convenient

(.&) :: Seq m r => r S -> r S -> m (r S)
(.|) :: Seq m r => r S -> r S -> m (r S)
(.^) :: Seq m r => r S -> r S -> m (r S)
(.==) :: Seq m r => r S -> r S -> m (r S)

(.&) = band
(.|) = bor
(.^) = bxor
(.==) = equ


reduce' _  [a]    = return a
reduce' op (a:as) = op a =<< reduce' op as
reduce' _  _      = error $ "reduce': need at least 1 operand"

one_of :: Seq m r => [r S] -> m (r S)
all_of :: Seq m r => [r S] -> m (r S)
one_of = reduce OR
all_of = reduce AND
