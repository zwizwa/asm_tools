let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          asm-tools-edsp =
            haskellPackagesNew.callPackage ./default.nix { };
          asm-tools =
            haskellPackagesNew.callPackage ./../asm-tools/default.nix { };
        };
      };
    };
  };

  pkgs = import ../nixpkgs { inherit config; };

in
  pkgs.haskellPackages.asm-tools-edsp

