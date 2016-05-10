{ mkDerivation, base, base64-bytestring, blaze-builder, bytestring
, conduit, conduit-extra, connection, containers, data-default, dns
, either, exceptions, iproute, lifted-base, monad-control
, monad-logger, stdenv, text, tls, transformers, transformers-base
, xml-conduit, xml-types
}:
mkDerivation {
  pname = "yaxmpp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder bytestring conduit
    conduit-extra connection containers data-default dns either
    exceptions iproute lifted-base monad-control monad-logger text tls
    transformers transformers-base xml-conduit xml-types
  ];
  executableHaskellDepends = [
    base bytestring connection dns either exceptions lifted-base
    monad-logger text transformers
  ];
  description = "Yet another XMPP implementation for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
