# Wrapper Makefile.
# See https://github.com/Gabriel439/haskell-nix
# This needs nix, cabal2nix, nix-prefetch-git:
# $ curl https://nixos.org/nix/install | sh
# $ nix-env -i cabal-install
# $ nix-env -i cabal2nix
# $ nix-env -i nix-prefetch-git

all: compile

clean:
	rm -f result *~

# Release build
compile: default.nix release.nix
	nix-build release.nix
	ls -l result/bin/asm_tools
default.nix: asm_tools.cabal
	cabal2nix . >$@

# Shell with build deps
NIX_SHELL := nix-shell --attr env release.nix
shell: default.nix
	$(NIX_SHELL)
repl-test-rtl: default.nix
	$(NIX_SHELL) --run "cabal repl test:test-rtl"

# Cabal is smarter about recompiling only part of the project for
# incremental development.
cabal-configure: default.nix
	$(NIX_SHELL) --run "cabal configure"
cabal-test: default.nix
	$(NIX_SHELL) --run "cabal test --log=/dev/stdout"



test: cabal-test






