{ mkDerivation, aeson, attoparsec, attoparsec-iso8601, base
, bytestring, conduit, conduit-extra, containers, crypton
, crypton-connection, data-default, dns, exceptions, haskeline
, iproute, irc, lib, monad-logger, network, primitive, QuickCheck
, ram, random, stm, stm-conduit, string-interpolate, stringprep
, tasty, tasty-quickcheck, text, text-show, time, tls, transformers
, unliftio, unliftio-core, uuid, vector, xml-conduit, xml-types
, yaml
}:
mkDerivation {
  pname = "yaxmpp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec attoparsec-iso8601 base bytestring conduit
    conduit-extra containers crypton crypton-connection data-default
    dns exceptions iproute monad-logger network primitive ram random
    string-interpolate stringprep text text-show time tls transformers
    unliftio unliftio-core uuid vector xml-conduit xml-types
  ];
  executableHaskellDepends = [
    aeson attoparsec attoparsec-iso8601 base bytestring conduit
    conduit-extra containers crypton crypton-connection data-default
    dns exceptions haskeline iproute irc monad-logger network primitive
    ram random stm stm-conduit string-interpolate stringprep text
    text-show time tls transformers unliftio unliftio-core uuid vector
    xml-conduit xml-types yaml
  ];
  testHaskellDepends = [
    aeson attoparsec attoparsec-iso8601 base bytestring conduit
    conduit-extra containers crypton crypton-connection data-default
    dns exceptions iproute monad-logger network primitive QuickCheck
    ram random string-interpolate stringprep tasty tasty-quickcheck
    text text-show time tls transformers unliftio unliftio-core uuid
    vector xml-conduit xml-types
  ];
  description = "Yet another XMPP implementation for Haskell";
  license = lib.licenses.bsd3;
}
