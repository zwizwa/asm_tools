#!/bin/bash

# FIXME: remove this.  too specific to kzoo setup.

[ -z "$1" ] && echo "usage: $0 <host>" && exit 1

PRJ=f_soc
#PRJ=f_blink

ssh $1 <<EOF
set -x
export PATH=/i/tom/git/icestorm/iceprog:\$PATH
cd /i/tom/asm_tools
iceprog -S $PRJ.ct256.bin
iceprog -x $PRJ.imem.bin
EOF


