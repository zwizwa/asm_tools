Low-level macro languages embedded in Haskell

- Misc assembly weaver & emulation tools for TI PRU-ICSS development.
  Basic idea is to use the PRU's predictable timing for control tasks
  that would normally require an FPGA.

- A 'tagless final' Sequential Logic (RTL) language.  Targets Verilog
  directly, and MyHDL, which translates to Verilog, VHDL.  A fast
  Template Haskell + Strict State Threads step is included for
  QuickCheck-based simulation.

- A stack CPU + embedded Forth/Assembler hybrid

- Misc tools  (VCD, netlists)

This is work in progress.  Functionality is added as needed.


