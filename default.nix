{ mkDerivation, attoparsec, base, base64-bytestring, blaze-builder
, bytestring, conduit, conduit-extra, connection, containers
, data-default, dns, either, exceptions, interpolatedstring-perl6
, iproute, lifted-base, monad-control, monad-logger, stdenv
, string-combinators, stringprep, text, tls, transformers
, transformers-base, xml-conduit, xml-types
}:
mkDerivation {
  pname = "yaxmpp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring blaze-builder bytestring conduit
    conduit-extra connection containers data-default dns either
    exceptions interpolatedstring-perl6 iproute lifted-base
    monad-control monad-logger string-combinators stringprep text tls
    transformers transformers-base xml-conduit xml-types
  ];
  executableHaskellDepends = [
    base bytestring connection dns either exceptions lifted-base
    monad-logger text transformers
  ];
  description = "Yet another XMPP implementation for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
