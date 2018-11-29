#!/bin/bash

# Small wrapper for zwizwa.
# FIXME: Find a better way to standardize this.

[ -z "$1" ] && echo "usage: <hostname> [<iceprog_arg> ...]" && exit 1

HOST=$1
shift

if [ "$HOST" == localhost ]; then
    exec iceprog "$@"
fi

ssh -T $HOST <<EOF
#set -x
export PATH=/i/tom/git/icestorm/iceprog:\$PATH
cd /i/tom/asm_tools
which iceprog
iceprog $@
EOF
