{-# LANGUAGE Strict #-}

-- | XEP-0300: Use of Cryptographic Hash Functions in XMPP — algorithm
-- registry, parsing/emitting of @\<hash xmlns="urn:xmpp:hashes:2"/\>@,
-- and convenience hashers built on @crypton@.
module Network.XMPP.XEP.Hash (
  HashAlgo,
  HashValue,
  hashesNS,
  hashesName,
  XMPPHashAlgorithm (..),
  SomeXMPPHashAlgorithm (..),
  someXMPPHashName,
  supportedHashAlgos,
  hashLazy,
  hashLazyBase64,
  hashElement,
  parseHashElement,
) where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Text.XML
import Text.XML.Cursor hiding (element)

import Network.XMPP.XML

-- | Hash algorithm name (e.g. "sha-1", "sha-256") as it appears on the wire.
type HashAlgo = Text

-- | Base64-encoded hash value.
type HashValue = Text

hashesNS :: Text
hashesName :: Text -> Name
(hashesNS, hashesName) = namePair "urn:xmpp:hashes:2"

{- | A 'HashAlgorithm' that has a registered XEP wire name. Provides the
two-way bridge between a type-level algorithm and the @algo@ attribute
string.
-}
class (Hash.HashAlgorithm a) => XMPPHashAlgorithm a where
  hashXMPPName :: Proxy a -> Text

instance XMPPHashAlgorithm Hash.SHA1 where hashXMPPName _ = "sha-1"
instance XMPPHashAlgorithm Hash.SHA256 where hashXMPPName _ = "sha-256"
instance XMPPHashAlgorithm Hash.SHA512 where hashXMPPName _ = "sha-512"
instance XMPPHashAlgorithm Hash.SHA3_256 where hashXMPPName _ = "sha3-256"
instance XMPPHashAlgorithm Hash.SHA3_512 where hashXMPPName _ = "sha3-512"
instance XMPPHashAlgorithm Hash.Blake2b_256 where hashXMPPName _ = "blake2b-256"
instance XMPPHashAlgorithm Hash.Blake2b_512 where hashXMPPName _ = "blake2b-512"

-- | Existential wrapper used to put differently-typed hashers in one map.
data SomeXMPPHashAlgorithm = forall a. (XMPPHashAlgorithm a) => SomeXMPPHashAlgorithm (Proxy a)

-- | Project the wire name out of a 'SomeXMPPHashAlgorithm'.
someXMPPHashName :: SomeXMPPHashAlgorithm -> HashAlgo
someXMPPHashName (SomeXMPPHashAlgorithm p) = hashXMPPName p

{- | Hash algorithms we know how to compute, keyed by their XEP wire name.
Sites that need a fixed list of algorithms (sending our own caps) should
look up here and 'fail' on a miss at construction time; sites processing
peer-advertised algorithms (validating caps) should look up and silently
skip on a miss.
-}
supportedHashAlgos :: Map HashAlgo SomeXMPPHashAlgorithm
supportedHashAlgos =
  M.fromList
    [ entry (Proxy :: Proxy Hash.SHA1)
    , entry (Proxy :: Proxy Hash.SHA256)
    , entry (Proxy :: Proxy Hash.SHA512)
    , entry (Proxy :: Proxy Hash.SHA3_256)
    , entry (Proxy :: Proxy Hash.SHA3_512)
    , entry (Proxy :: Proxy Hash.Blake2b_256)
    , entry (Proxy :: Proxy Hash.Blake2b_512)
    ]
 where
  entry :: forall a. (XMPPHashAlgorithm a) => Proxy a -> (HashAlgo, SomeXMPPHashAlgorithm)
  entry p = (hashXMPPName p, SomeXMPPHashAlgorithm p)

-- | Hash a lazy 'BSL.ByteString' with the given algorithm; raw digest bytes.
hashLazy :: SomeXMPPHashAlgorithm -> BSL.ByteString -> ByteString
hashLazy (SomeXMPPHashAlgorithm p) = go p
 where
  go :: forall a. (XMPPHashAlgorithm a) => Proxy a -> BSL.ByteString -> ByteString
  go _ bs = BA.convert (Hash.hashlazy bs :: Hash.Digest a)

-- | Hash a lazy 'BSL.ByteString' and base64-encode the digest as 'Text'.
hashLazyBase64 :: SomeXMPPHashAlgorithm -> BSL.ByteString -> HashValue
hashLazyBase64 some bs = T.decodeUtf8 (BA.convertToBase BA.Base64 (hashLazy some bs))

-- | Render a @\<hash algo="..."\>...\</hash\>@ element.
hashElement :: HashAlgo -> HashValue -> Element
hashElement algo val = element (hashesName "hash") [("algo", algo)] [NodeContent val]

{- | Parse a single @\<hash xmlns="urn:xmpp:hashes:2" algo="..."\>...\</hash\>@
element. Returns 'Nothing' if @algo@ is missing or the content is empty.
-}
parseHashElement :: Element -> Maybe (HashAlgo, HashValue)
parseHashElement h = do
  algo <- getAttr "algo" h
  let val = mconcat $ fromElement h $/ content
  if val == "" then Nothing else Just (algo, val)
