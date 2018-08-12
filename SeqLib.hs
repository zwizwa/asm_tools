{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}

module SeqLib where
import Seq
import Names                
import Control.Monad
import Control.Applicative
import Data.Key(Zip(..),zipWith, zip)
import Prelude hiding (zipWith, zip)
import Data.Bits hiding (bit)

-- Some general guidelines.
--
-- . Don't write library routines that take monadic values as
--   arguments, unless they behave as macros.
--
-- . The exception seems to be switch.  The reason?  It allows the use
--   of local variables in the clauses to make the code more readable.
--   Even if the signals could be reused, they likely won't be,
--   because the behavior "belongs" to that clause.



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
reg' :: Seq m r => SType -> (r S -> m (r S)) -> m (r S)
reg' t f = do closeReg [t] $ \[r] -> do r' <- f r ; return ([r'], r)



-- Convenient shortcut for test probe
(.=) :: Seq m r => String -> r S -> m ()
(.=) = probe



-- Some simple building blocks

inc :: Seq m r => r S -> m (r S)
inc c = add c 1

dec :: Seq m r => r S -> m (r S)
dec c = sub c 1

counter :: Seq m r => SType -> m (r S)
counter t = reg' t inc

delay :: Seq m r => r S -> m (r S)
delay x = do
  t <- stype x
  reg' t $ \_ -> return x

edge d = do
  d0 <- delay d
  d `bxor` d0


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
  reg' t $ sync' (constant t) i


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
  tr <- stype r
  ti <- stype i
  let SInt (Just r_bits) _ = tr
      SInt (Just i_bits) _ = ti
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
  reg' t $ add x


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

  -- FIXME: parameterize
  -- data_count_bits = nb_bits nb_data_bits
  -- oversample_count_bits = 3 :: Int --  8x oversampling
  -- oversample = 2 ^ oversample_count_bits
  
  [idle, start, bits, stop] = [0,1,2,3] :: [r S]

  half_bit =  3 :: r S
  one_bit  =  7 :: r S
  all_bits = 63 :: r S

  t_state = SInt (Just 2) 0  -- state
  t_count = SInt (Just 6) 0  -- sub-bit counter

  sm :: m (r S, r S)
  sm = closeReg [t_state,t_count] update

  update :: [r S] -> m ([r S], (r S, r S))
  update [state, count] = do
    -- Subcircuits hoisted out of conditional.
    count1   <- dec count
    phase    <- slice count (Just 3) 0
    countz   <- count `equ` 0
    bitClock <- phase `equ` 0
    
    (bc:wc:regs') <- switch state [
      (idle,  do
          state' <- if' rx idle start
          -- skip half a bit to sample mid-bit
          return [0, 0, state', half_bit]),
      (start, do
          -- skip start bit
          [state', count'] <- ifs countz [bits, all_bits] [state, count1]
          return [0, 0, state', count']),
      (bits,  do
          -- send out bit clock mid-bit
          [state', count'] <- ifs countz [stop, one_bit] [state, count1]
          return [bitClock, 0, state', count']),
      (stop,  do
          -- only send out the word clock if stop bit is 1
          wordClock <- bitClock `band` rx
          [state', count'] <- ifs countz [idle, 0] [state, count1]
          return [0, wordClock, state', count'])]
      -- FIXME: not reached.  How to express mutex states?
      (return [0, 0, 0, 0])

    -- test probes
    "rx_in" .= rx
    "rx_bc" .= bc
    "rx_wc" .= wc
      
    return (regs', (bc,wc))
--    return (regs', rx:bc:wc:regs')

-- async_receive_sample nb_bits rx = do
--   (_:bc:wc:_) <- d_async_receive_sample nb_bits rx
--   return (bc,wc)



async_receive nb_bits i = do
  (bc,wc) <- async_receive_sample nb_bits i
  sr <- withClockEnable bc $ shiftReg ShiftRight (bits nb_bits) i
  -- return (sr:regs)
  return (wc,sr)


-- FIXME: To test this, use a program sequencer, because it requires
-- response to the done bit.

async_transmit bitClock (wordClock, txData) = do
  (SInt (Just n) _) <- stype txData
  closeReg [SInt (Just $ n+1) (-1),
            SInt (Just $ nb_bits $ n+2) 0] $
    \[shiftReg, cnt] -> do

      -- $(names [| [shiftReg, cnt] |])  -- Reify variable names
      

      -- Outputs are a function of previous state.
      out  <- slice' shiftReg 1 0
      done <- cnt `equ` 0

      newframe <- conc txData (cbit 0)
      shifted  <- conc (cbit 1) =<< slice' shiftReg (n+1) 1
      cntDec'  <- dec cnt
      cntDec   <- if' done 0 cntDec'

      -- Not handling wordClock and bitClock at the same time makes it
      -- easier to express and reduces data path length.
      [shiftReg', cnt'] <- cond
        [(wordClock, [newframe, 10]),
         (bitClock,  [shifted,  cntDec])]
        [shiftReg, cnt]

      "tx_bc"   .= bitClock
      "tx_wc"   .= wordClock
      "tx_in"   .= txData
      "tx_done" .= done
      "tx_out"  .= out
      
      -- return ([shiftReg', cnt'],
      --         [out, done,
      --          wordClock, bitClock, shiftReg', cnt'])

      return ([shiftReg', cnt'], (out, done))



-- SPI

-- It's assumed that signals are properly double-D synchronized.  Note
-- that there might be a delay difference between clock and data, so 

-- Sample on rising edge only.  I beleive this is the same as the iCE40.

sync_clock sclk = do
  e <- edge sclk
  e `band` sclk

-- Assume wordsize is power of two so we can roll around the counter.
-- FIXME: It's easier to reuse the counter and transfer directly into
-- the memory.
sync_receive nb_bits cs sclk sdata = do
    sc <- sync_clock sclk
    bc <- band sc =<< inv cs
    out@(wc, w) <- deser ShiftLeft (bits nb_bits) (bc, sdata)
    "sclk"  .= sclk
    "sdata" .= sdata
    "s_bc"  .= bc
    "s_wc"  .= wc
    "s_w"   .= w
    return out

-- sync_receive_sample nb_bits@16 cs sclk = do
--   closeReg [bits nb_bits] $ \[count] -> do
--     sc     <- sync_clock sclk
--     bc     <- band sc =<< inv cs
--     count1 <- inc count
--     count' <- if' bc count1 count
--     wc     <- count' `equ` (-1)
--     return ([count'], (bc, wc))
  

-- Seserializer for converting bit streams to word streams.
deser :: Seq m r => ShiftDir -> SType -> (r S, r S) -> m (r S, r S)
deser dir t_sr@(SInt (Just nb_bits) _) (bitClock, bitVal) = do
  let t_sr' = t_nb_bits t_sr
      n_max = constant $ SInt Nothing $ nb_bits - 1
  (wordClock', wordVal) <-
    closeRegEn bitClock [t_sr, t_sr'] $
    \[sr,n] -> do
      wc  <- n `equ` n_max
      n1  <- inc n
      n'  <- if' wc 0 n1
      sr' <- shiftUpdate dir sr bitVal
      return ([sr',n'], (wc, sr'))
  wordClock <- bitClock `band` wordClock'
  return (wordClock, wordVal)




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

-- open interface, memory closed elsewhere
fifoPtr :: Seq m r => SType -> r S -> m (r S)
fifoPtr t wc = do
  closeReg [t] $ \[a] -> do
    a1 <- inc a
    a' <- if' wc a1 a
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
    (SInt mbits _) <- stype sel
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

