{
  nixpkgs ? import <nixpkgs> {},
  compiler ? "default",
}: let
  inherit (nixpkgs) pkgs;

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

  drvShell = pkgs.haskell.lib.overrideCabal drv (args: {
    libraryToolDepends =
      args.libraryToolDepends or []
      ++ [
        pkgs.alejandra
        haskellPackages.hlint
        haskellPackages.fourmolu
      ];
  });
in
  if pkgs.lib.inNixShell
  then drvShell.env
  else drv
