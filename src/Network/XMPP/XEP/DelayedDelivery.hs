{-# LANGUAGE Strict #-}

-- | XEP-0203: Delayed Delivery
module Network.XMPP.XEP.DelayedDelivery (
  DelayInfo (..),
  delayedDeliveryPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Data.List (partition)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Text.XML
import Text.XML.Cursor hiding (element)

import qualified Data.Registry as Reg
import Data.Time.XMPP
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stream
import Network.XMPP.XML

_delayNS :: Text
delayName :: Text -> Name
(_delayNS, delayName) = namePair "urn:xmpp:delay"

-- | Delayed delivery information (XEP-0203).
data DelayInfo = DelayInfo
  { delayFrom :: Maybe Text
  , delayStamp :: ZonedTime
  , delayReason :: Maybe Text
  }
  deriving (Show)

parseDelay :: Element -> Maybe DelayInfo
parseDelay e
  | elementName e == delayName "delay" = do
      stampText <- getAttr "stamp" e
      case xmppZonedTime stampText of
        Left _ -> Nothing
        Right delayStamp ->
          let delayFrom = getAttr "from" e
              delayReason = case mconcat $ fromElement e $/ content of
                "" -> Nothing
                t -> Just t
           in Just DelayInfo {..}
  | otherwise = Nothing

delayToElement :: DelayInfo -> Element
delayToElement (DelayInfo {..}) =
  element
    (delayName "delay")
    (catMaybes [Just ("stamp", zonedTimeToXmpp delayStamp), fmap ("from",) delayFrom])
    (maybe [] (\r -> [NodeContent r]) delayReason)

-- * Plugin

data DelayedDeliveryPlugin = DelayedDeliveryPlugin

instance (MonadStream m) => Codec m Presence DelayedDeliveryPlugin where
  codecDecode _ pres =
    let (mdelay, raw') = extractDelay (presenceRaw pres)
        ext = presenceExtended pres
        ext' = maybe ext (\d -> Reg.insert d ext) mdelay
     in return $ pres {presenceRaw = raw', presenceExtended = ext'}

  codecEncode _ pres =
    let ext = presenceExtended pres
        (mdelay, ext') = Reg.pop (Proxy :: Proxy DelayInfo) ext
        raw = presenceRaw pres
        raw' = maybe raw (\d -> delayToElement d : raw) mdelay
     in return $ pres {presenceRaw = raw', presenceExtended = ext'}

instance (MonadStream m) => Codec m IMMessage DelayedDeliveryPlugin where
  codecDecode _ msg =
    let (mdelay, raw') = extractDelay (imRaw msg)
        ext = imExtended msg
        ext' = maybe ext (\d -> Reg.insert d ext) mdelay
     in return $ msg {imRaw = raw', imExtended = ext'}

  codecEncode _ msg =
    let ext = imExtended msg
        (mdelay, ext') = Reg.pop (Proxy :: Proxy DelayInfo) ext
        raw = imRaw msg
        raw' = maybe raw (\d -> delayToElement d : raw) mdelay
     in return $ msg {imRaw = raw', imExtended = ext'}

extractDelay :: [Element] -> (Maybe DelayInfo, [Element])
extractDelay elems =
  let (delayElems, rest) = partition (\e -> elementName e == delayName "delay") elems
   in (listToMaybe $ mapMaybe parseDelay delayElems, rest)

delayedDeliveryPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
delayedDeliveryPlugin pluginsRef = do
  pCodecs <- presenceCodecs pluginsRef
  Codec.pushNewOrFailM DelayedDeliveryPlugin pCodecs
  mCodecs <- imCodecs pluginsRef
  Codec.pushNewOrFailM DelayedDeliveryPlugin mCodecs
