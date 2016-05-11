{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      xml-conduit = overrideCabal super.xml-conduit (oldAttrs: {
        version = "1.3.5";
        sha256 = "10yw8jiksw21jgwqjjd1ixflavcblisgkp9qq3ba05vadc35lqr5";
      });
      # https://github.com/vincenthz/hs-tls/issues/140
      tls = self.tls_1_3_4;
      cryptonite = self.cryptonite_0_10;
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
