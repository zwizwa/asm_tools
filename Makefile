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
	rm -f result *~ x_*

.PHONY: myhdl_test
myhdl_test:  x_blink_fpga.ct256.bin




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
cabal-configure: default.nix
	$(NIX_SHELL) --run "cabal configure --disable-optimization"
cabal-test: default.nix
	$(NIX_SHELL) --run "cabal test --log=/dev/stdout"




test: cabal-test


# MyHDL

# FPGA: MyHDL + yosys + arachne-pnr
MYHDL:=$(shell readlink -f myhdl)


%.myhdl: %.py run_myhdl.py $(MYHDL) Makefile
	PYTHONPATH="$(MYHDL)" python3 run_myhdl.py $* $< >$@.tmp
	mv $@.tmp $@

# apt-get install gtkwave
# gtkwave module.vcd


# Logic synthesis.  Same for all ice40.
%.blif: %.v
	yosys -p "synth_ice40 -blif $@" $<  >$*.yosys.log

# Place and route, one for each device,package type.
%.qn84.asc: %.blif %.pcf
	arachne-pnr -P qn84 -d 1k -p $*.pcf $< -o $@ >$*.qn84.pnr
%.ct256.asc: %.blif %.pcf
	arachne-pnr -P ct256 -d 8k -p $*.pcf $< -o $@ >$*.ct256.pnr

# Some fake fanout.  Do check that the file exists before touching.
%.v: %.myhdl
	[ -f "$@" ] && touch $@
%.vhd: %.myhdl
	[ -f "$@" ] && touch $@
%.pcf: %.myhdl
	[ -f "$@" ] && touch $@
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

%.iceprog: %.bin
	iceprog





