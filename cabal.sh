#!/bin/sh
ENV_FILE=$(dirname $0)/ghc.env
if [ -f $ENV_FILE ]; then
    OLD_ENV=$(cat $ENV_FILE)
fi
NEW_ENV=$(readlink -f $(which ghc))

if [ "$OLD_ENV" != "$NEW_ENV" ]; then
    echo "$0: running in new environment; removing dist/"
    echo "OLD_ENV=$OLD_ENV"
    echo "NEW_ENV=$NEW_ENV"
    echo "$NEW_ENV" >$ENV_FILE
    rm -rf dist
fi

exec cabal "$@"


# Note: Why is this needed?

# 1. While cabal2nix takes care of all dependencies, it uses full nix
#    package rebuilds to do so.  This is slow.
#
# 2. To support incremental builds, cabal is executed in a nix shell.
#    The build products are in dist/
#
# 3. However, cabal cannot track changes made to the environment,
#    e.g. if source changes but version number doesn't, so we delete
#    the full build cache whenever the environment changes.

# e.g.:
#  OLD_ENV=/nix/store/wqyfhh5l3mxbgk8ihaybn7l5iq041pvn-ghc-8.2.2-with-packages/bin/ghc
#  NEW_ENV=/nix/store/33k57l2350bikhhc9z8acn8f5wcxlihr-ghc-8.2.2-with-packages/bin/ghc


# FIXME: Manage this more explicitly in the Makefile instead.  Touch a
# file whenver the environment changes.

