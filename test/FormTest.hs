module FormTest (tests) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.HUnit
import Text.XML

import Network.XMPP.XEP.Forms (Form (..), parseForm)

parseXmlFragment :: BSLC.ByteString -> Element
parseXmlFragment bs = documentRoot (parseLBS_ def bs)

testParseFormMultiValueField :: TestTree
testParseFormMultiValueField = testCase "parseForm preserves separate <value> entries in multi-value fields" $ do
  let xml = "<x xmlns=\"jabber:x:data\" type=\"result\"><field var=\"FORM_TYPE\" type=\"hidden\"><value>http://example.test</value></field><field var=\"addresses\" type=\"list-multi\"><value>a@x</value><value>b@y</value></field></x>"
  case parseForm (parseXmlFragment xml) of
    Right (Just (Just "http://example.test", Form fields)) ->
      M.lookup "addresses" fields @?= Just ["a@x", "b@y"]
    other -> assertFailure $ "unexpected parseForm result: " ++ show other

testParseFormSingleValueField :: TestTree
testParseFormSingleValueField = testCase "parseForm extracts single-value fields" $ do
  let xml = "<x xmlns=\"jabber:x:data\" type=\"result\"><field var=\"FORM_TYPE\" type=\"hidden\"><value>urn:test</value></field><field var=\"max\" type=\"text-single\"><value>42</value></field></x>"
  case parseForm (parseXmlFragment xml) of
    Right (Just (Just "urn:test", Form fields)) ->
      M.lookup "max" fields @?= Just ["42"]
    other -> assertFailure $ "unexpected parseForm result: " ++ show other

testParseFormNonForm :: TestTree
testParseFormNonForm = testCase "parseForm returns Nothing for non-form elements" $ do
  let xml = "<query xmlns=\"http://jabber.org/protocol/disco#info\"/>"
  parseForm (parseXmlFragment xml) @?= Right Nothing

tests :: TestTree
tests =
  testGroup
    "Forms"
    [ testParseFormMultiValueField
    , testParseFormSingleValueField
    , testParseFormNonForm
    ]
