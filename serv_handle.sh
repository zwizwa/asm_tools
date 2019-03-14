#!/bin/bash
# To be used as:
# ./shell --run ./serv.sh

SERV=$(dirname $0)/.serv
[ -d $SERV ] && echo "$SERV already exists" && exit 1
PID=$SERV/pid
CMD=$SERV/cmd
OUT=$SERV/out
cleanup() {
    rm -rf $SERV
    exit 0
}
mkdir -p $SERV
chmod 700 $SERV
mkfifo $CMD
mkfifo $OUT
echo $$ >$PID
trap cleanup EXIT INT TERM

while echo "$0: waiting: $(readlink -f $CMD)" ; do
    (while read line; do 
         echo "$0: $line"
         (echo "$0: Entering directory '$(readlink -f .)'"
          $line) >$OUT 2>&1;
    done)<$CMD
done

# To run in emacs compile mode:
# echo 'cabal build' >cmd ; cat out





