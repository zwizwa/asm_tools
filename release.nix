let
#  pkgs = import <nixpkgs> { };
  pkgs = import ./nixpkgs { };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
