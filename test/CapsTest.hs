module CapsTest (tests) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit

import Network.XMPP.Language (localizedFromText)
import Network.XMPP.XEP.Capabilities (caps1Bytes, caps2Bytes)
import Network.XMPP.XEP.Disco (
  DiscoEntity (..),
  DiscoIdentity (..),
  emptyDiscoEntity,
 )
import Network.XMPP.XEP.Forms (Form (..))
import Network.XMPP.XEP.Hash (hashLazyBase64, supportedHashAlgos)

prosodyEntity :: DiscoEntity
prosodyEntity =
  emptyDiscoEntity
    { discoIdentities =
        M.fromList
          [ (DiscoIdentity "server" "im", Just (localizedFromText "Prosody"))
          , (DiscoIdentity "store" "file", Just (localizedFromText "HTTP File Upload"))
          ]
    , discoFeatures =
        S.fromList
          [ "http://jabber.org/protocol/commands"
          , "http://jabber.org/protocol/disco#info"
          , "http://jabber.org/protocol/disco#items"
          , "jabber:iq:last"
          , "jabber:iq:private"
          , "jabber:iq:register"
          , "jabber:iq:roster"
          , "jabber:iq:version"
          , "msgoffline"
          , "urn:xmpp:blocking"
          , "urn:xmpp:carbons:2"
          , "urn:xmpp:carbons:rules:0"
          , "urn:xmpp:extdisco:1"
          , "urn:xmpp:extdisco:2"
          , "urn:xmpp:http:upload"
          , "urn:xmpp:http:upload:0"
          , "urn:xmpp:ping"
          , "urn:xmpp:time"
          , "vcard-temp"
          ]
    , discoForms =
        M.fromList
          [
            ( "http://jabber.org/network/serverinfo"
            , Form $
                M.fromList
                  [ ("abuse-addresses", ["mailto:abuse@fmap.me"])
                  , ("admin-addresses", ["mailto:admin@fmap.me", "xmpp:admin@fmap.me"])
                  ]
            )
          , ("urn:xmpp:http:upload", Form $ M.singleton "max-file-size" ["104857600"])
          , ("urn:xmpp:http:upload:0", Form $ M.singleton "max-file-size" ["104857600"])
          ]
    }

expectedCaps1String :: BSLC.ByteString
expectedCaps1String =
  "server/im//Prosody<\
  \store/file//HTTP File Upload<\
  \http://jabber.org/protocol/commands<\
  \http://jabber.org/protocol/disco#info<\
  \http://jabber.org/protocol/disco#items<\
  \jabber:iq:last<\
  \jabber:iq:private<\
  \jabber:iq:register<\
  \jabber:iq:roster<\
  \jabber:iq:version<\
  \msgoffline<\
  \urn:xmpp:blocking<\
  \urn:xmpp:carbons:2<\
  \urn:xmpp:carbons:rules:0<\
  \urn:xmpp:extdisco:1<\
  \urn:xmpp:extdisco:2<\
  \urn:xmpp:http:upload<\
  \urn:xmpp:http:upload:0<\
  \urn:xmpp:ping<\
  \urn:xmpp:time<\
  \vcard-temp<\
  \http://jabber.org/network/serverinfo<\
  \abuse-addresses<mailto:abuse@fmap.me<\
  \admin-addresses<mailto:admin@fmap.me<xmpp:admin@fmap.me<\
  \urn:xmpp:http:upload<max-file-size<104857600<\
  \urn:xmpp:http:upload:0<max-file-size<104857600<"

testCaps1BytesProsody :: TestTree
testCaps1BytesProsody = testCase "caps1Bytes produces XEP-0115 verification string for Prosody disco" $
  caps1Bytes prosodyEntity @?= expectedCaps1String

testCaps1HashProsody :: TestTree
testCaps1HashProsody = testCase "caps1 sha-1 hash matches fmap.me's advertised ver" $ do
  sha1 <- case M.lookup "sha-1" supportedHashAlgos of
    Just a -> return a
    Nothing -> assertFailure "sha-1 algo missing from supportedHashAlgos" >> error "unreachable"
  hashLazyBase64 sha1 (caps1Bytes prosodyEntity) @?= "II2M3+ur7sQOyYxF7wa4A6zvrpE="

testCaps2BytesIncludesMultiValue :: TestTree
testCaps2BytesIncludesMultiValue = testCase "caps2Bytes preserves multi-value separators" $ do
  let bs = caps2Bytes prosodyEntity
      payload = BSLC.unpack bs
      adminMarker = "admin-addresses\x1fmailto:admin@fmap.me\x1fxmpp:admin@fmap.me\x1f"
  assertBool ("expected admin-addresses block in: " ++ payload) (adminMarker `isInfixOf` payload)
 where
  isInfixOf needle haystack = any (needle `prefixOf`) (tailsOf haystack)
  prefixOf [] _ = True
  prefixOf _ [] = False
  prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys
  tailsOf [] = [[]]
  tailsOf xs@(_ : rest) = xs : tailsOf rest

tests :: TestTree
tests =
  testGroup
    "Capabilities"
    [ testCaps1BytesProsody
    , testCaps1HashProsody
    , testCaps2BytesIncludesMultiValue
    ]
