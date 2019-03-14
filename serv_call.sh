#!/bin/bash
[ -z "$1" ] && echo "$0 <cmd> [<arg> ...]" && exit 1
SERV=$(dirname $0)/.serv
[ ! -d $SERV ] && echo "$0: no $SERV dir" && exit 1
echo "$0: server pid $(cat $SERV/pid)"
echo "$@" >$SERV/cmd ; cat $SERV/out

# For a nix context, run "./shell.sh --run ./serv_handle.sh" in another window.
# Note that multiple access to the fifos is not supported.

# In fact, the only driver for writing this code was to maintain the
# state of the server process between calls, i.e. a nix shell context.

# To support multiple access, use a queueing mechanism like tsp, or
# wrap the scrip in an Erlang process.



