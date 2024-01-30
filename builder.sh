#!/bin/sh
source $stdenv/setup

# All build products go here
echo "out=$out"
mkdir -p $out
touch $out/FIXME
