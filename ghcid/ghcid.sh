#!/bin/bash
[ -z "$GHCID" ] && GHCID=~/.cabal/bin/ghcid
cd $(dirname $0)
exec $GHCID --command="make ghci" --test main



