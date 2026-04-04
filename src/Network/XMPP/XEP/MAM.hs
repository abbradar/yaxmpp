module Network.XMPP.XEP.MAM where

import Data.Text (Text)
import Text.XML (Name)

import Network.XMPP.XML

mamNS :: Text
mamName :: Text -> Name
(mamNS, mamName) = namePair "urn:xmpp:mam:2"
