Low-level macro languages embedded in Haskell

This is work in progress.  Functionality is added as needed.


- Misc assembly weaver & emulation tools for TI PRU-ICSS development.
  Basic idea is to use the PRU's predictable timing for control tasks
  that would normally require an FPGA.

- A 'tagless final' Sequential Logic (RTL) language.  Targets Verilog
  directly, and MyHDL, which translates to Verilog, VHDL.  A fast
  Template Haskell + Strict State Threads step is included for
  QuickCheck-based simulation.  A C target is availbl as well.

- A stack CPU + embedded Forth/Assembler hybrid implemented in Seq

- A CSP-style channel API

- Misc tools  (VCD, netlists)



Some notes regarding Seq:

- Seq uses applicative style: state machines are modeled as operators
  (monadic functions) that take inputs to outputs, with internal
  register loops hidden from the user.
  
  Seq operators are "pure", in the sense that they can be thought of
  as functions taking input sequences to output sequences, with the
  added constraint that the i->o relation is causal.
  
  Obviously they cannot be pure in the Haskell embedding because state
  is involved in the implementation, but at least the property is
  there to be used for reasoning and the construction of higher level
  abstractions.  This is the idea to prefer Applicative over Arrow.
  
  Compared to the traditional port style approach where you "connect"
  ports, the applicative style has less notiational overhead because
  outputs do not need to be named.  Combined with the idea of purity
  over sequences explained above, there is no need to have a separate
  instantiation phase: a state machine can just be used as any other
  i/o function which is the second notational advantage.
  
  This makes fine-grained abstraction much more concise, but comes at
  a price.  Any operation that defines a state machine will also need
  to be written in applicative style where the combinatorial update
  function is structured as a function from register outputs to
  register inputs (or more correctly expressed in the
  pure-operation-on-sequences view: as the function that maps the
  unit-delayed _sequence_ to the non-delayed sequence).  A state
  machine is then constructed by an explicit "close" operator, which
  essentially hides the registers from view.
  
  This tends to shift the forces that drive modularization: it makes
  it really easy to create lots of simple modules as they all just
  look like functions, but it can make it harder to create "ball of
  mud" state machines that have update functions spanning a large
  number of registers in a way that is not easy to factor into a
  combination of smaller state machines.  In some way the language
  forces you to modularize to a much finer degree.  Up to now I have
  found this to be a net plus, but it takes some getting used to.

- Seq is sequential, i.e. has implicit clocks.  I am aware that in
  general this is not what you want for digital logic, due to there
  often being different clock domains.  However, the application that
  is driving the requirements for Seq is relatively simple and has
  only a single clock domain.  The intention is to later put a layer
  inbetween HDL output and Seq that does allow representation of
  explicit clocks and asynchronous logic.  This layer is likely going
  to be based on Verilog directly, orchestrated by FuseSoC.

- Seq does not represent bit vector size at the Haskell type level.
  While at times this feels like a missed opportunity, I did not have
  the skill or the time to make that work.  It is (was) my assessment
  that the machinery necessary to do so is just too heavyweight for
  encoding just bit vectors.  Better encoding can be done for
  abstractions built on top of Seq using ad hoc phantom types.

- The Seq CPU is really simple.  It is definitely not general purpose.
  However I do find it useful.  It does not seem to need pipelining at
  the clock rates I'm using it at, and it is terribly convenient to
  have function nesting and loops as opposed to writing custom state
  machine sequencers.  Also, being able to express both machine code
  and the instruction decoder, bus architecture etc.. in the same
  Haskell module opens up a lot of possibilities.  Additionally, CPU
  programs can be used to write QuickCheck tests for sequential logic
  peripherals.
  
  







