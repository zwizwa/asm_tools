redo-ifchange default.nix
exec nix-shell --attr env release.nix "$@"
