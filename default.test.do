redo-ifchange $(find -name '*.hs')
redo-ifchange default.nix

# Don't build when the command loop server is active.
if [ -f .serv/pid ]; then
    echo "WARNING: pid=$(cat pid), not building." >&2
    touch $3
    exit 0
fi

LOG=$(readlink -f $3)
nix-shell --attr env release.nix  --run "./cabal.sh test $2 --log=$LOG" >&2
cat $LOG >&2




