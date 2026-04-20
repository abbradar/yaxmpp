{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.EntityTime (
  EntityTimePlugin,
  getEntityTimePlugin,
  getEntityTime,
  entityTimePlugin,
) where

import Control.Monad.IO.Class
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Typeable (Typeable)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Data.Time.XMPP
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML

timeNS :: Text
timeName :: Text -> Name
(timeNS, timeName) = namePair "urn:xmpp:time"

newtype EntityTimePlugin m = EntityTimePlugin
  { entityTimePluginSession :: StanzaSession m
  }

instance (Typeable m) => DiscoInfoProvider (EntityTimePlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton timeNS

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (EntityTimePlugin m) where
  tryHandle _ (InRequestIQ {iriType = IQGet, iriChildren = [req]})
    | elementName req == timeTag = do
        tz <- liftIO getCurrentTimeZone
        utime <- liftIO getCurrentTime
        let result =
              [ ("tzo", timeZoneToXmpp tz)
              , ("utc", utcTimeToXmpp utime)
              ]
        return $ Just $ IQResult [element timeTag [] $ map (\(name, value) -> NodeElement $ element (timeName name) [] [NodeContent value]) result]
   where
    timeTag = timeName "time"
  tryHandle _ _ = return Nothing

getEntityTimePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (EntityTimePlugin m)
getEntityTimePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (EntityTimePlugin m)) $ pluginsHooksSet pluginsRef

getEntityTime :: (MonadStream m) => EntityTimePlugin m -> XMPPAddress -> (Either StanzaError ZonedTime -> m ()) -> m ()
getEntityTime EntityTimePlugin {entityTimePluginSession} addr handler =
  stanzaRequest
    entityTimePluginSession
    OutRequestIQ
      { oriTo = Just addr
      , oriIqType = IQGet
      , oriChildren = [closedElement (timeName "time")]
      }
    $ \resp -> handler $ case resp of
      Left e -> Left e
      Right [r]
        | elementName r == timeName "time"
        , [tzoStr] <- getEntry r "tzo"
        , Right tz <- xmppTimeZone tzoStr
        , [utcStr] <- getEntry r "utc"
        , Right utime <- xmppZonedTime utcStr
        , -- Not strictly required but is a MUST by XEP
          zonedTimeZone utime == utc ->
            Right $ utcToZonedTime tz $ zonedTimeToUTC utime
      _ -> Left $ badRequest "invalid entity time response"
 where
  getEntry r name = fromElement r $/ XC.element (timeName name) &/ content

entityTimePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
entityTimePlugin pluginsRef = do
  let plugin :: EntityTimePlugin m = EntityTimePlugin {entityTimePluginSession = pluginsSession pluginsRef}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsIQHandlers pluginsRef
  addDiscoInfo pluginsRef plugin
