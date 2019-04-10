.PHONY: all clean
all:
	@echo see Makefile
clean:
	rm -rf dist default.nix

# Generate nix setup from Haskell project file
default.nix: asm-tools-ghci.cabal
	cabal2nix . >$@
ghci: default.nix
	nix-shell --attr env release.nix --run "ghci main.hs"
