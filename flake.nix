# Following example from
# https://github.com/srid/haskell-flake
# Runs into error:

# error: Trying to retrieve system-dependent attributes for input nixpkgs, but this input is not a flake. Perhaps flake = false was added to the input declarations by mistake, or you meant to use a different input, or you meant to use plain old inputs, not inputs'.
# (use '--show-trace' to show detailed location information)

{
  description = "asm_tools";
  inputs = {
    # nixpkgs.url = github:NixOS/nixpkgs/23.11;
    nixpkgs.url = github:NixOS/nixpkgs/85f1ba3e51676fa8cc604a3d863d729026a6b8eb;
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    
  };


  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`, e.g. from Hackage
          packages = {
            aeson.source = "1.5.0.0"; # Hackage version
          };

          # my-haskell-package development shell configuration
          devShell = {
            hlsCheck.enable = false;
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        devShells.default = pkgs.mkShell {
          name = "my-haskell-package custom development shell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            # other development tools.
          ];
        };
      };
    };
}
