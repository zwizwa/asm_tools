# Wrapper Makefile.
# See https://github.com/Gabriel439/haskell-nix
# This needs nix, cabal2nix, nix-prefetch-git:
# $ curl https://nixos.org/nix/install | sh
# $ nix-env -i cabal-install
# $ nix-env -i cabal2nix
# $ nix-env -i nix-prefetch-git

# Note that cabal2nix's goal is to build releases using nix.  This
# does not support incremental builds.  See cabal.sh for notes on how
# incremental devlopment builds are implemented on top of "nix shell".

all: compile

.SECONDARY:

clean:
	rm -f result *~ x_* *.v *.vhd *.bin *.blif *.asc f_*.py x_*.py \
		*.compile *.tmp *.vcd *.vcd.* f_*.bin f_*.v *.log ghc.env *.elf *.d *.pcf default.nix
	rm -rf __pycache__ dist

.PHONY: myhdl_test
myhdl_test:  f_blink.ct256.bin f_soc.ct256.bin

# Used to trigger rebuilds.
HS := $(shell find Data -name '*.hs') \
	$(shell find Language -name '*.hs') \
	$(shell find examples -name '*.hs') \
	$(shell find test -name '*.hs')


# Release build
compile: default.nix release.nix
	nix-build release.nix
default.nix: asm_tools.cabal
	cabal2nix . >$@

# Shell with build deps
NIX_SHELL := nix-shell --attr env release.nix
shell: default.nix
	$(NIX_SHELL)
repl-test-seq: default.nix
	$(NIX_SHELL) --run "./cabal.sh repl test:test-seq"
repl-test-emu: default.nix
	$(NIX_SHELL) --run "./cabal.sh repl test:test-emu"
repl-test-edif: default.nix
	$(NIX_SHELL) --run "./cabal.sh repl test:test-edif"

# Cabal is smarter about recompiling only part of the project for
# incremental development.
configure: default.nix
	$(NIX_SHELL) --run "./cabal.sh configure --disable-optimization"
cabal-test: default.nix
	$(NIX_SHELL) --run "./cabal.sh test --log=/dev/stdout"


# FPGA top level code generation.

# Nesting build systems can be problematic: dependencies often get
# lost, leading to slow coarse-grained builds which are a pain to use
# during development.
# 
# After many iterations, I've settled on the following approach.  The
# requirement is that for each generated file, make should know how to
# build it with minimal effort.
#
# Instead of using one multi-target build rule that maps all inputs to
# all outputs, we use a generator that can produce a dependency list
# as a Makefile, since make already has support for generating include
# files, e.g. as used for using GCC to generated .h -> .c file deps.
#
# There is one such file for each generator executable.  See
# asm_tools.cabal for the list of generators.  Look at the .d files to
# get a better idea of how everything is built.
-include f_soc.d
.PHONY: f_soc
f_soc: $(f_soc_files)

# These .d files are created in a uniform way by the generator
# executable.  Dump the file into the build log, leaving breadcrumbs.
f_%.d: f_%.elf
	./f_$*.elf >$@
	cat $@

# The generator executables themselves are created by cabal.  We don't
# have access to the deps, so use all the .hs files in the project.
f_%.elf: default.nix $(HS)
	$(NIX_SHELL) --run "./cabal.sh build f_$*"
	rm -f $@
	ln dist/build/f_$*/f_$* $@



# FIXME: This associates a single program to each top level module.
# How to specify multiple programs?  Make can't express "tables" of
# dependencies.


t_cosim: default.nix
	make -C vpi
	$(NIX_SHELL) --run "./cabal.sh build t_cosim"
	export SEQ_COSIM=$$(readlink -f vpi)/cosim ; ./dist/build/t_cosim/t_cosim

csv2vcd: default.nix
	$(NIX_SHELL) --run "./cabal.sh build csv2vcd"

dump: default.nix
	$(NIX_SHELL) --run "./cabal.sh build dump"

# Inidividual tests
test-seq-qc: default.nix
	$(NIX_SHELL) --run "./cabal.sh test seq-qc --log=/dev/stdout"
test-seq-x: default.nix
	$(NIX_SHELL) --run "./cabal.sh test seq-x --log=/dev/stdout"
test-pru: default.nix
	$(NIX_SHELL) --run "./cabal.sh test pru --log=/dev/stdout"
test-edif: default.nix
	$(NIX_SHELL) --run "./cabal.sh test edif --log=/dev/stdout"
test-sat: default.nix
	$(NIX_SHELL) --run "./cabal.sh test sat --log=/dev/stdout"




# MyHDL

# FPGA: MyHDL + yosys + arachne-pnr
MYHDL:=$(shell readlink -f myhdl)

# FIXME: side effect files .v -> .vhd
# %.v %.vhd: %.py run_myhdl.py $(MYHDL) Makefile
# 	PYTHONPATH="$(MYHDL)" python3 run_myhdl.py $* $<

# apt-get install gtkwave
# gtkwave module.vcd


VERILOG_LIB := \
	verilog/reset.v

# Logic synthesis.  Same for all ice40.
%.blif: %.v $(VERILOG_LIB) Makefile
	yosys -p "synth_ice40 -blif $@" $(VERILOG_LIB) $<  >$*.yosys.log
	tail -n35 $*.yosys.log 

# Place and route.  One rule for each board type (there might be
# different boards with the same FPGA).
%.breakout.asc: %.blif %.breakout.pcf
	arachne-pnr -P ct256 -d 8k -p $*.breakout.pcf $< -o $@
%.fbr.asc: %.blif %.fbr.pcf
	arachne-pnr -P qn84 -d 1k -p $*.fbr.pcf $< -o $@



%.ct256.time: %.pcf %.ct256.asc
	icetime -p $*.pcf -o $*.ct256.nl.v -P ct256 -d hx8k -t $*.ct256.asc

build/fpga_trigger_gen.py: .stamp.generate
	[ -f "$@" ] && touch $@
build/testbench_gen.py: .stamp.generate
	[ -f "$@" ] && touch $@

# Bitstream
%.bin: %.asc
	icepack $< $@

# For flashing iCEblink40-LP1K eval board EEPROM
%.icedude: %.bin  
	PATH=~/.cabal/bin:$$PATH iCEDude -U flash:w:$<

# SRAM programming.  Note that the jumpers J6 need to be in the
# correct position (horizontal when holding the board up with USB
# connector at the bottom).
%.iceprog.zoe: %.bin
	./iceprog.sh zoe -S $<

# Note: this uses a modified version of iceprog to upload the SRAM
# image over the same SPI interface as used for the iCE40 image.
%.ramprog.zoe: %.bin
	./iceprog.sh zoe -x $<

# Examples:
# tom@panda:~/asm_tools$ make f_soc.breakout.iceprog.zoe
# tom@panda:~/asm_tools$ make f_soc.prog3.ramprog.zoe
