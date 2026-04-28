{-# LANGUAGE Strict #-}

-- | XEP-0203: Delayed Delivery
module Network.XMPP.XEP.DelayedDelivery (
  DelayInfo (..),
  tryParseDelay,
  delayedDeliveryPlugin,
) where

import Control.Monad.Logger
import Data.List (partition)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Text.XML
import Text.XML.Cursor hiding (element)

import qualified Data.Registry as Reg
import Data.Time.XMPP
import Network.XMPP.Address (FullJID, XMPPAddress)
import Network.XMPP.Filter (Filter (..))
import qualified Network.XMPP.Filter as Filter
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stanza (StanzaError)
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

-- * Plugin

data DelayedDeliveryPlugin = DelayedDeliveryPlugin

-- | Receive-only filter: clients don't stamp @\<delay/\>@ themselves
-- (servers/intermediaries do it).
instance (MonadStream m) => Filter m FullJID Presence StanzaError DelayedDeliveryPlugin where
  filterReceive _ _ pres = case extractDelay (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0203 delayed delivery: #{err}|]
      return $ Right pres
    Right (mdelay, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\d -> Reg.insert d ext) mdelay
       in return $ Right $ pres {presenceRaw = raw', presenceExtended = ext'}
  filterSend _ _ pres = return $ Right pres

instance (MonadStream m) => Filter m XMPPAddress IMMessage StanzaError DelayedDeliveryPlugin where
  filterReceive _ _ msg = case extractDelay (imRaw msg) of
    Left err -> do
      $(logError) [i|XEP-0203 delayed delivery: #{err}|]
      return $ Right msg
    Right (mdelay, raw') ->
      let ext = imExtended msg
          ext' = maybe ext (\d -> Reg.insert d ext) mdelay
       in return $ Right $ msg {imRaw = raw', imExtended = ext'}
  filterSend _ _ msg = return $ Right msg

extractDelay :: [Element] -> Either String (Maybe DelayInfo, [Element])
extractDelay elems =
  let (delayElems, rest) = partition (\e -> elementName e == delayName "delay") elems
   in case delayElems of
        [] -> Right (Nothing, rest)
        (e : _) -> (\d -> (Just d, rest)) <$> parseDelay e

delayedDeliveryPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
delayedDeliveryPlugin pluginsRef = do
  pp <- getPresencePlugin pluginsRef
  Filter.pushNewOrFailM DelayedDeliveryPlugin (presencePluginFilters pp)
  imp <- getIMPlugin pluginsRef
  Filter.pushNewOrFailM DelayedDeliveryPlugin (imPluginFilters imp)
