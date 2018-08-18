#!/bin/bash

# FIXME: remove this.  too specific to kzoo setup.

[ -z "$1" ] && echo "usage: $0 <host>" && exit 1

ssh $1 <<EOF
set -x
export PATH=/i/tom/git/icestorm/iceprog:\$PATH
cd /i/tom/asm_tools
iceprog -S f_soc.ct256.bin
iceprog -x f_soc.imem.bin
EOF


