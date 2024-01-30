#!/bin/sh
HERE=$(dirname "$0")
cd $HERE
nix develop --print-build-logs --command make -C "$HERE"/asm-tools-seq



