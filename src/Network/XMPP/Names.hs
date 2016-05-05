module Network.XMPP.Names where

import qualified Data.Text as T
import Data.XML.Types

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
