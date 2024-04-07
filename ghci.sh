#!/bin/sh
cd $(dirname "$0")
ghci \
    -iasm-tools \
    -iasm-tools-seq \
    -iasm-tools-seq-dt \
    -iasm-tools-pru \
    -iasm-tools-dsp \
    -iasm-tools-top \
    "$@"

