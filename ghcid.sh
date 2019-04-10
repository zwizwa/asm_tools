#!/bin/bash

# Notes:
#
# - Running 'cabal repl' doesn't seem to work properly.
#
# - Don't make this too configurable.  Just edit this file when
#   current development setup changes.
#
# - This supports the (modified) ghcid.el wrapper, which calls us with
#   arguments -h <height>.

MAIN=test/seq-qc.hs

cd $(dirname $0)
make default.nix
nix-shell --attr env release.nix --run "ghcid $* --command=\"ghci $MAIN\" --test main"
