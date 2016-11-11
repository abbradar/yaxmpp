{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
      aeson = self.aeson_1_0_2_1;
      xml-conduit = self.callPackage ./xml/xml-conduit { };
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
