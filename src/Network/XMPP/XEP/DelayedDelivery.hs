{-# LANGUAGE Strict #-}

-- | XEP-0203: Delayed Delivery
module Network.XMPP.XEP.DelayedDelivery (
  DelayInfo (..),
  tryParseDelay,
  delayedDeliveryPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.Monad.Logger
import Data.List (partition)
import Data.Maybe
import Data.Proxy
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Text.XML
import Text.XML.Cursor hiding (element)

import qualified Data.Registry as Reg
import Data.Time.XMPP
import Network.XMPP.Address (FullJID, XMPPAddress)
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

parseDelay :: Element -> Either String DelayInfo
parseDelay e = do
  stampText <- maybe (Left "missing stamp attribute on <delay>") Right $ getAttr "stamp" e
  delayStamp <- case xmppZonedTime stampText of
    Left err -> Left $ "invalid stamp on <delay>: " <> err
    Right t -> Right t
  let delayFrom = getAttr "from" e
      delayReason = case mconcat $ fromElement e $/ content of
        "" -> Nothing
        t -> Just t
  return DelayInfo {..}

tryParseDelay :: Element -> Maybe DelayInfo
tryParseDelay e
  | elementName e == delayName "delay" = either (const Nothing) Just $ parseDelay e
  | otherwise = Nothing

delayToElement :: DelayInfo -> Element
delayToElement (DelayInfo {..}) =
  element
    (delayName "delay")
    (catMaybes [Just ("stamp", zonedTimeToXmpp delayStamp), fmap ("from",) delayFrom])
    (maybe [] (\r -> [NodeContent r]) delayReason)

-- * Plugin

data DelayedDeliveryPlugin = DelayedDeliveryPlugin

instance (MonadStream m) => Codec m FullJID Presence DelayedDeliveryPlugin where
  codecDecode _ _ pres = case extractDelay (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0203 delayed delivery: #{err}|]
      return pres
    Right (mdelay, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\d -> Reg.insert d ext) mdelay
       in return $ pres {presenceRaw = raw', presenceExtended = ext'}

  codecEncode _ _ pres =
    let ext = presenceExtended pres
        (mdelay, ext') = Reg.pop (Proxy :: Proxy DelayInfo) ext
        raw = presenceRaw pres
        raw' = maybe raw (\d -> delayToElement d : raw) mdelay
     in return $ pres {presenceRaw = raw', presenceExtended = ext'}

instance (MonadStream m) => Codec m XMPPAddress IMMessage DelayedDeliveryPlugin where
  codecDecode _ _ msg = case extractDelay (imRaw msg) of
    Left err -> do
      $(logError) [i|XEP-0203 delayed delivery: #{err}|]
      return msg
    Right (mdelay, raw') ->
      let ext = imExtended msg
          ext' = maybe ext (\d -> Reg.insert d ext) mdelay
       in return $ msg {imRaw = raw', imExtended = ext'}

  codecEncode _ _ msg =
    let ext = imExtended msg
        (mdelay, ext') = Reg.pop (Proxy :: Proxy DelayInfo) ext
        raw = imRaw msg
        raw' = maybe raw (\d -> delayToElement d : raw) mdelay
     in return $ msg {imRaw = raw', imExtended = ext'}

extractDelay :: [Element] -> Either String (Maybe DelayInfo, [Element])
extractDelay elems =
  let (delayElems, rest) = partition (\e -> elementName e == delayName "delay") elems
   in case delayElems of
        [] -> Right (Nothing, rest)
        (e : _) -> (\d -> (Just d, rest)) <$> parseDelay e

delayedDeliveryPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
delayedDeliveryPlugin pluginsRef = do
  pp <- getPresencePlugin pluginsRef
  Codec.pushNewOrFailM DelayedDeliveryPlugin (presencePluginCodecs pp)
  imp <- getIMPlugin pluginsRef
  Codec.pushNewOrFailM DelayedDeliveryPlugin (imPluginCodecs imp)
