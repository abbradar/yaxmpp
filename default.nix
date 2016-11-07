{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, bytestring, conduit, conduit-extra, connection
, containers, data-default, dns, either, exceptions
, interpolatedstring-perl6, iproute, lifted-base, monad-control
, monad-logger, stdenv, stringprep, text, tls, transformers
, transformers-base, xml-conduit, xml-types
}:
mkDerivation {
  pname = "yaxmpp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-builder bytestring
    conduit conduit-extra connection containers data-default dns either
    exceptions interpolatedstring-perl6 iproute lifted-base
    monad-control monad-logger stringprep text tls transformers
    transformers-base xml-conduit xml-types
  ];
  executableHaskellDepends = [
    base bytestring connection dns either exceptions
    interpolatedstring-perl6 lifted-base monad-logger text transformers
  ];
  description = "Yet another XMPP implementation for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
