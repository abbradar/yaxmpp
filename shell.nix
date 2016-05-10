{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # https://github.com/snoyberg/xml/pull/85
      xml-conduit = overrideCabal super.xml-conduit (oldAttrs: {
        src =
          let src0 = pkgs.fetchgit {
            url = "git://github.com/abbradar/xml";
            rev = "ccc63ba4a253571775892ec4c2458a874e3da743";
            sha256 = "0d5vc6jq1p3p0qgi8a4720bkwr0nxndjnicfl9plv416ri67i5sf";
          };
          in "${src0}/xml-conduit";
      });
      # https://github.com/vincenthz/hs-tls/issues/140
      tls = self.tls_1_3_4;
      cryptonite = self.cryptonite_0_10;
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
