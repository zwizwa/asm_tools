{
  description = "asm_tools";
  inputs = {
    nixpkgs.url = github:zwizwa/nixpkgs/exo;
  };

  outputs = { self, nixpkgs }:
    let system = "x86_64-linux";
        pkgs = import nixpkgs {
          inherit system;
        };
        buildInputs = (with pkgs; [
        ]);
    in
  {
    packages.${system}.default =
      pkgs.stdenv.mkDerivation {
        name = "asm-tools";
        src = self;
        libraryHaskellDepends = with pkgs; [
          array base binary bytestring containers free keys mtl network
          parsec process QuickCheck set-extra split system-argv0
          system-filepath template-haskell temporary unix
        ];
      };

    # New standard flake approach
    devShells.${system}.default =
      pkgs.mkShell {
        packages = buildInputs;
        shellHook = ''
          PS1="(asm_tools) \u@\h:\w\$ "
        '';          
      };
  };
}
