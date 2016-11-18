{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, bytestring, conduit, conduit-extra, connection
, containers, data-default-class, dns, either, exceptions
, interpolatedstring-perl6, iproute, irc-conduit, lifted-base
, monad-control, monad-logger, stdenv, stm-conduit, stringprep
, text, time, tls, transformers, transformers-base, xml-conduit
, xml-types, yaml
}:
mkDerivation {
  pname = "yaxmpp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-builder bytestring
    conduit conduit-extra connection containers data-default-class dns
    either exceptions interpolatedstring-perl6 iproute lifted-base
    monad-control monad-logger stringprep text time tls transformers
    transformers-base xml-conduit xml-types
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra connection containers
    data-default-class dns either exceptions interpolatedstring-perl6
    irc-conduit lifted-base monad-logger stm-conduit text transformers
    yaml
  ];
  description = "Yet another XMPP implementation for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
