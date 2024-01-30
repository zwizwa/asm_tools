.PHONY: all clean
all:
	cabal build



# 20210130. Above is validated for new flake.nix build
# Below needs to be reviewed.

# .PHONY: all clean shell release
# all: release
# 	@echo see Makefile
# clean:
# 	rm -rf dist default.nix

# default.nix: asm-tools-seq.cabal
# 	cabal2nix . >$@

# shell: default.nix
# 	nix-shell --attr env release.nix

# release: default.nix release.nix
# 	nix-build release.nix
