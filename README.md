Low-level macro languages embedded in Haskell

This is work in progress.  Functionality is added as needed.


- Misc assembly weaver & emulation tools for TI PRU-ICSS development.
  Basic idea is to use the PRU's predictable timing for control tasks
  that would normally require an FPGA.

- A 'tagless final' Sequential Logic (RTL) language.  Targets Verilog
  directly, and MyHDL, which translates to Verilog, VHDL.  A fast
  Template Haskell + Strict State Threads step is included for
  QuickCheck-based simulation.

- A stack CPU + embedded Forth/Assembler hybrid

- Misc tools  (VCD, netlists)



Some notes regarding Seq:

- It is sequential, i.e. has implicit clocks.  I am aware that in
  general this is not what you want, due to there often being
  different clock domains.  However, the application that is driving
  the requirements for Seq is relatively simple and has only a single
  clock domain.  The intention is to later put a layer inbetween HDL
  output and Seq that does allow representation of explicit clocks and
  asynchronous logic.

- Seq does not represent bit vector size at the Haskell type level.  I
  believe this is a missed opportunity, but I did not know how to make
  it work in the time I had available.  Fixing that likey requires to
  create a version 2, and refactor the library.  This is not for now.

- The Seq CPU is really simple.  It is definitely not general purpose.
  However I do find it serves a purpose.  It does not seem to need
  pipelining at the clock rates I'm using it at, and it is terribly
  convenient to have function nesting and loops as opposed to writing
  custom state machine sequencers.  Also, being able to express both
  machine code and the instruction decoder, bus architecture etc.. in
  the same Haskell module opens up a lot of possibilities.
  Additionally, CPU programs can be used to write QuickCheck tests for
  sequential logic peripherals.

In general I really like this approach, but it is definitely not
finished.





