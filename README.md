Low-level macro languages embedded in Haskell
=============================================

Context
-------

First, what this is not:

- A general purpose Hardware Description Language.  For that, see:

  - https://clash-lang.org/
  
  - https://hackage.haskell.org/package/chalmers-lava2000
  
  - https://bluespec.com/
  
  - And many other Haskell and non-Haskell projects

- A general purpose C replacement.  For that, see:

  - https://hackage.haskell.org/package/ivory
  
  - And many other Haskell and non-Haskell projects generating C, and
    of course the Rust programming language.
  
So what is it?  This project explores the idea of *substrate
independence* for low level control and signal processing problems. I
am essentially using only one trick:
http://okmij.org/ftp/tagless-final/index.html, which in my adaptation
can be summarized as:

- Create a Haskell type class implementing a minimal abstract
  instruction set that is sufficient to implement a particular
  algorithm.  This is the abstract substrate.
  
- Where possible, try to organize the substrates hierarchically,
  e.g. create new substrates by composing smaller ones.
  
- Implement algorithms on top of those substrates.

- Create interpretations for the substrates.  In practice this always
  contains two interpretations: a compiler to some standard format (C,
  Verilog), and an emulator that provides instrumentation to allow
  implementing constraint checks.  I.e. a test bench.
  


It is currently not the intention to write a general purpose Haskell
library.  There are too many degrees of freedom, and I found it more
appropriate to create custom type classes for the problem at hand.
The hope is that eventually some reusable code will fall out of this,
and that the approach can be translated to a recipe book on top of
existing Haskell tools for low level programming.  In addition I try
to keep the Haskell trickery to a minimum.

In the current stage of development, the purpose of publishing this
repository is twofold: to support existing proprietary projects
written using this code, and to provide an experience report of using
the tagless-final approach in the field.

If you are interested to discuss this subject, please contact me
directly.  I try my best to explain things but I find I do not always
succeed to be clear as I am very much still trying to find a good way
to structure the code.


Contents
--------

- Misc assembly weaver & emulation tools for TI PRU-ICSS development.
  We use the PRU's predictable timing for control tasks that would
  normally require an FPGA.  Validation is done by a custum emulator
  that can check timing properties of a program.

- A sequential logic language.  Targets are Verilog, MyHDL, C, Haskell
  ST monad, and a (slow) Haskell emulator.  The Haskell ST target is
  fast enough to create test benches, which are implemented as
  property based tests using QuickCheck.

- A stack CPU + embedded Forth/Assembler hybrid implemented in Seq.
  This is a first attempt to work towards substrate independence,
  i.e. allow a CPU implemented in Seq to implement Seq again.

- Misc glue tools (VCD logic trace dumps, netlist files, ...)


Notes regarding Seq
-------------------

- Seq is sequential, i.e. has implicit clocks.  I am aware that in
  general this is not what you want for digital logic, due to there
  often being different clock domains.  However, the application that
  is driving the requirements for Seq is relatively simple and has
  only a single clock domain.  The intention is to later put a layer
  inbetween HDL output and Seq that does allow representation of
  explicit clocks and asynchronous logic.  This layer is likely going
  to be based on Verilog directly, orchestrated by FuseSoC.
  
- There is a CSP-style channel API.  The idea here is to provide a
  communication abstraction that can be used both for logic running in
  the same cock domain, but can also be used to cross clock domains in
  an abstract way. (This is very much work in progress.)

- Seq does not represent bit vector size at the Haskell type level.
  While at times this feels like a missed opportunity, I did not have
  the skill or the time to make that work.  It is (was) my assessment
  that the machinery necessary to do so is just too heavyweight for
  encoding just bit vectors.  Better encoding can be done for
  abstractions built on top of Seq using ad hoc phantom types.
  
- Seq operators are "pure", in the sense that they can be thought of
  as functions taking input sequences to output sequences, with the
  added constraint that the i->o relation is causal. However, the
  implementation of the compiler needs state, so I have opted to use
  the traditional monadic approach, and not use the "observable
  sharing" tricks that are used in many other DSLs, which allows logic
  to be captured as pure Haskell code.  The trade-off here is quite
  clear to me now: notation is a bit clumsy because it is monadic and
  it is hard to explain to people that do not have experience with
  Haskell or monadic embedding (translated: why can't I use
  expressions?), but the implementation of the compilers and
  interpreters becomes much more straightforward, and at this point
  that is the most important design constraint.
  
- Seq uses applicative style: state machines are modeled as operators
  (monadic functions) that take inputs to outputs. Compared to the
  traditional port style approach where you "connect" ports, the
  applicative style has less notiational overhead because outputs do
  not need to be named.  Combined with the idea of purity over
  sequences explained above, there is no need to have a separate
  instantiation phase: a state machine can just be used as any other
  i/o function which is the second notational advantage.  There seems
  to be a general consensus in the Haskell community to prefer
  Applicative over Arrow.
  
- I have opted to keep register and memory feedback explicit.  Any
  operation that defines a state machine will need to pass a state
  transition function to a "close" operator.  I have found this
  explicit style more clear and more flexible to retarget to other
  substrates than more implicit approaches as used e.g. in Clash.

- Note that the applicative style with explicit close operator tends
  to shift the forces that drive modularization: it makes it really
  easy to create lots of simple modules as they all just look like
  functions, but it can make it harder to create "ball of mud" state
  machines that have update functions spanning a large number of
  registers in a way that is not easy to factor into a combination of
  smaller state machines.  In some way the Seq language forces you to
  modularize to a much finer degree. I have learned that some people
  are really not a fan of this approach.  Up to now I have found this
  to be a net plus and maybe the biggest lesson to learn about how to
  structure code better, but it takes some getting used to.
  
  


Notes regarding the Forth CPU
-----------------------------

- The CPU in Seq is a simple proof-of-concept design.  It is
  definitely not general purpose.  However I do find it useful.  It
  does not seem to need pipelining at the clock rates and on th
  hardware I'm using it, and it is terribly convenient to have
  function nesting and loops as opposed to writing custom state
  machine sequencers.
  
- Being able to express both machine code and the instruction decoder,
  bus architecture etc.. in the same Haskell module opens up a lot of
  possibilities.  Additionally, CPU programs can be used to write
  QuickCheck tests for sequential logic peripherals.
