{
  mkDerivation,
  aeson,
  attoparsec,
  attoparsec-iso8601,
  base,
  base64-bytestring,
  bytestring,
  conduit,
  conduit-extra,
  containers,
  crypton,
  crypton-connection,
  data-default,
  dns,
  exceptions,
  haskeline,
  hpack,
  iproute,
  irc,
  lib,
  monad-logger,
  network,
  primitive,
  random,
  stm,
  stm-conduit,
  string-interpolate,
  stringprep,
  text,
  text-show,
  time,
  tls,
  transformers,
  unliftio,
  unliftio-core,
  uuid,
  xml-conduit,
  xml-types,
  yaml,
}:
mkDerivation {
  pname = "yaxmpp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    attoparsec
    attoparsec-iso8601
    base
    base64-bytestring
    bytestring
    conduit
    conduit-extra
    containers
    crypton
    crypton-connection
    data-default
    dns
    exceptions
    iproute
    monad-logger
    network
    primitive
    random
    string-interpolate
    stringprep
    text
    text-show
    time
    tls
    transformers
    unliftio
    unliftio-core
    uuid
    xml-conduit
    xml-types
  ];
  libraryToolDepends = [hpack];
  executableHaskellDepends = [
    aeson
    attoparsec
    attoparsec-iso8601
    base
    base64-bytestring
    bytestring
    conduit
    conduit-extra
    containers
    crypton
    crypton-connection
    data-default
    dns
    exceptions
    haskeline
    iproute
    irc
    monad-logger
    network
    primitive
    random
    stm
    stm-conduit
    string-interpolate
    stringprep
    text
    text-show
    time
    tls
    transformers
    unliftio
    unliftio-core
    uuid
    xml-conduit
    xml-types
    yaml
  ];
  prePatch = "hpack";
  description = "Yet another XMPP implementation for Haskell";
  license = lib.licenses.bsd3;
}
