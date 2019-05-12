let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          asm-tools-grid =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs = import ../nixpkgs { inherit config; };

in
  pkgs.haskellPackages.asm-tools-grid

