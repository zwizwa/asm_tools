#!/bin/sh
set -xe
cd $(dirname "$0")
cabal build
# exec ../dist-newstyle//build/x86_64-linux/ghc-9.6.5/asm-tools-c-1.0.0/x/test-smc/build/test-smc/test-smc "$@"
exec ../dist-newstyle//build/x86_64-linux/ghc-9.6.5/asm-tools-c-1.0.0/x/test-lcd/build/test-lcd/test-lcd "$@"

