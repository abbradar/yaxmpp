module Network.XMPP.XML where

import Text.Read
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Blaze.ByteString.Builder as BB
import Text.XML
import qualified Text.XML.Unresolved as XMLU
import qualified Text.XML.Stream.Render as XMLR
import Data.Conduit
import qualified Data.Conduit.List as CL
import Text.XML.Cursor (Cursor)
import qualified Text.XML.Cursor as XC

nsName :: T.Text -> T.Text -> Name
nsName ns name = Name name (Just ns) Nothing

namePair :: T.Text -> (T.Text, T.Text -> Name)
namePair name = (name, nsName name)

xmlNS :: T.Text
xmlName :: T.Text -> Name
(xmlNS, xmlName) = namePair "http://www.w3.org/XML/1998/namespace"

streamNS :: T.Text
streamName :: T.Text -> Name
(streamNS, streamName) = namePair "http://etherx.jabber.org/streams"

jcNS :: T.Text
jcName :: T.Text -> Name
(jcNS, jcName) = namePair "jabber:client"

jsNS :: T.Text
jsName :: T.Text -> Name
(jsNS, jsName) = namePair "jabber:server"

smNS :: Text
smName :: Text -> Name
(smNS, smName) = namePair "urn:xmpp:sm:3"

bindNS :: Text
bindName :: Text -> Name
(bindNS, bindName) = namePair "urn:ietf:params:xml:ns:xmpp-bind"

element :: Name -> [(Name, Text)] -> [Node] -> Element
element name attrs nodes = Element { elementName = name
                                   , elementAttributes = M.fromList attrs
                                   , elementNodes = nodes
                                   }

closedElement :: Name -> Element
closedElement name = element name [] []

showElement :: Element -> Text
showElement e = T.decodeUtf8 $ BB.toByteString $ mconcat bs
  where bs = runIdentity $ CL.sourceList (XMLU.elementToEvents $ toXMLElement e) $$ XMLR.renderBuilder def =$= CL.consume

getAttr :: Name -> Element -> Maybe Text
getAttr n e = M.lookup n $ elementAttributes e

readAttr :: Read a => Name -> Element -> Maybe a
readAttr n e = do
  attr <- getAttr n e
  readMaybe $ T.unpack attr

fromElement :: Element -> Cursor
fromElement = XC.fromNode . NodeElement
