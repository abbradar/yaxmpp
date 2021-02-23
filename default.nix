{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, bytestring, conduit, conduit-extra, connection
, containers, data-default-class, dns, exceptions, haskeline, hpack
, iproute, irc, lib, monad-logger, primitive, stm, stm-conduit
, string-interpolate, stringprep, text, text-show, time, tls
, transformers, unliftio, unliftio-core, uuid, xml-conduit
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
    exceptions iproute monad-logger primitive string-interpolate
    stringprep text text-show time tls transformers unliftio
    unliftio-core uuid xml-conduit xml-types
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-builder bytestring
    conduit conduit-extra connection containers data-default-class dns
    exceptions haskeline iproute irc monad-logger primitive stm
    stm-conduit string-interpolate stringprep text text-show time tls
    transformers unliftio unliftio-core uuid xml-conduit xml-types yaml
  ];
  prePatch = "hpack";
  description = "Yet another XMPP implementation for Haskell";
  license = lib.licenses.bsd3;
}
