{-# LANGUAGE Strict #-}

-- | XEP-0297: Stanza Forwarding
module Network.XMPP.XEP.Forwarding (
  forwardNS,
  Forwarded (..),
  parseForwarded,
) where

import Data.Maybe
import Data.Text (Text)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Network.XMPP.Stanza (StanzaError, badRequest)
import Network.XMPP.XEP.DelayedDelivery (DelayInfo)
import qualified Network.XMPP.XEP.DelayedDelivery as DD
import Network.XMPP.XML

forwardNS :: Text
forwardName :: Text -> Name
(forwardNS, forwardName) = namePair "urn:xmpp:forward:0"

data Forwarded = Forwarded
  { fwdDelay :: Maybe DelayInfo
  , fwdMessage :: Element
  }
  deriving (Show)

{- | Parse a @\<forwarded\>@ element, extracting an optional @\<delay\>@ and the inner @\<message\>@.
Returns @Nothing@ if the element is not @\<forwarded\>@, or @Just (Left err)@ if malformed.
-}
parseForwarded :: Element -> Maybe (Either StanzaError Forwarded)
parseForwarded e
  | elementName e == forwardName "forwarded" =
      let cur = fromElement e
          fwdDelay = listToMaybe $ mapMaybe DD.parseDelay $ cur $/ curAnyElement
       in Just $ case listToMaybe $ cur $/ XC.element (jcName "message") &| curElement of
            Nothing -> Left $ badRequest "parseForwarded: no <message> in <forwarded>"
            Just fwdMessage -> Right Forwarded {..}
  | otherwise = Nothing
