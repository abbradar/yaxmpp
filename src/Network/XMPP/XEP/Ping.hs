{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Ping (
  PingPlugin,
  getPingPlugin,
  sendPing,
  pingPlugin,
) where

import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.XML (Name, elementName)

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML

pingNS :: Text
pingName :: Text -> Name
(pingNS, pingName) = namePair "urn:xmpp:ping"

newtype PingPlugin m = PingPlugin
  { pingPluginSession :: StanzaSession m
  }

instance (Typeable m) => DiscoInfoProvider (PingPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton pingNS

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (PingPlugin m) where
  tryHandle _ (InRequestIQ {iriType = IQGet, iriChildren = [req]})
    | elementName req == pingName "ping" =
        return $ Just $ IQResult []
  tryHandle _ _ = return Nothing

getPingPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (PingPlugin m)
getPingPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (PingPlugin m)) $ pluginsHooksSet pluginsRef

{- | Send a ping to the given address. Calls the handler with 'Right ()' on
success (result IQ) or 'Left err' on error.
-}
sendPing :: (MonadStream m) => PingPlugin m -> XMPPAddress -> (Either StanzaError () -> m ()) -> m ()
sendPing PingPlugin {pingPluginSession} addr handler =
  stanzaRequest
    pingPluginSession
    OutRequestIQ
      { oriTo = Just addr
      , oriIqType = IQGet
      , oriChildren = [closedElement (pingName "ping")]
      }
    $ \resp -> handler $ case resp of
      Left e -> Left e
      Right _ -> Right ()

pingPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
pingPlugin pluginsRef = do
  let plugin :: PingPlugin m = PingPlugin {pingPluginSession = pluginsSession pluginsRef}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM plugin iqHandlers
  addDiscoInfo pluginsRef plugin
