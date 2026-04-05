{
  description = "Yet another XMPP implementation for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {pkgs, ...}: let
        haskellPackages = pkgs.haskellPackages;
        yaxmpp = haskellPackages.callPackage ./default.nix {};
      in {
        packages = {
          default = yaxmpp;
          yaxmpp = yaxmpp;
        };

        formatter = pkgs.alejandra;

        devShells.default =
          (pkgs.haskell.lib.overrideCabal yaxmpp (args: {
            libraryToolDepends =
              (args.libraryToolDepends or [])
              ++ [
                haskellPackages.hlint
                haskellPackages.fourmolu
                haskellPackages.hpack
                haskellPackages.cabal-install
              ];
          })).env;
      };
    };
}
