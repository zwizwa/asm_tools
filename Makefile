# Wrapper Makefile.
# See https://github.com/Gabriel439/haskell-nix
# This needs nix, cabal2nix, nix-prefetch-git:
# $ curl https://nixos.org/nix/install | sh
# $ nix-env -i cabal-install
# $ nix-env -i cabal2nix
# $ nix-env -i nix-prefetch-git


all: compile

.SECONDARY:

clean:
	rm -f result *~ x_* *.v *.vhd *.bin *.blif *.asc f_*.py x_*.py *.compile *.tmp *.vcd f_*.bin f_*.v
	rm -rf __pycache__ dist

.PHONY: myhdl_test
myhdl_test:  f_blink.ct256.bin f_soc.ct256.bin

HS := $(shell find -name '*.hs')



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
	$(NIX_SHELL) --run "cabal repl test:test-seq"
repl-test-emu: default.nix
	$(NIX_SHELL) --run "cabal repl test:test-emu"
repl-test-edif: default.nix
	$(NIX_SHELL) --run "cabal repl test:test-edif"

# Cabal is smarter about recompiling only part of the project for
# incremental development.
configure: default.nix
	$(NIX_SHELL) --run "cabal configure --disable-optimization"
test: default.nix
	$(NIX_SHELL) --run "cabal test --log=/dev/stdout"

# Target applications
f_%.v f_%.pcf: default.nix $(HS)
	$(NIX_SHELL) --run "cabal build f_$*"
	dist/build/f_$*/f_$*


# f_soc: default.nix
# 	$(NIX_SHELL) --run "cabal build f_soc"
# 	dist/build/f_soc/f_soc
# f_blink: default.nix
# 	$(NIX_SHELL) --run "cabal build f_blink"	
# 	dist/build/f_blink/f_blink

t_cosim: default.nix
	make -C vpi
	$(NIX_SHELL) --run "cabal build t_cosim"
	export SEQ_COSIM=$$(readlink -f vpi)/cosim ; ./dist/build/t_cosim/t_cosim

# Inidividual tests
test-seq-qc: default.nix
	$(NIX_SHELL) --run "cabal test seq-qc --log=/dev/stdout"
test-seq-x: default.nix
	$(NIX_SHELL) --run "cabal test seq-x --log=/dev/stdout"
test-pru: default.nix
	$(NIX_SHELL) --run "cabal test pru --log=/dev/stdout"
test-edif: default.nix
	$(NIX_SHELL) --run "cabal test edif --log=/dev/stdout"
test-sat: default.nix
	$(NIX_SHELL) --run "cabal test sat --log=/dev/stdout"

# # These need corresponding entries in the .cabal file
# f_%.py f_%.pcf f_%.imem.bin: f_%.hs $(HS)
# 	rm -f f_$*.imem.bin # workaround: openBinaryFile: resource exhausted (Resource temporarily unavailable)
# 	$(NIX_SHELL) --run "cabal test f_$* --log=/dev/stdout"




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

# Place and route, one for each device,package type.
%.qn84.asc: %.blif %.pcf
	arachne-pnr -P qn84 -d 1k -p $*.pcf $< -o $@
%.ct256.asc: %.blif %.pcf
	arachne-pnr -P ct256 -d 8k -p $*.pcf $< -o $@


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
%.iceprog: %.bin
	iceprog -S $<


