{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Ping (
  sendPing,
  pingPlugin,
) where

import Control.Monad
import qualified Data.Set as S
import Data.Text (Text)
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

data PingPlugin = PingPlugin

instance DiscoInfoProvider PingPlugin where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton pingNS

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse PingPlugin where
  tryHandle _ (InRequestIQ {iriType = IQGet, iriChildren = [req]})
    | elementName req == pingName "ping" =
        return $ Just $ IQResult []
  tryHandle _ _ = return Nothing

{- | Send a ping to the given address. Calls the handler with 'Right ()' on
success (result IQ) or 'Left err' on error.
-}
sendPing :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> (Either StanzaError () -> m ()) -> m ()
sendPing pluginsRef addr handler = do
  let sess = pluginsSession pluginsRef
  stanzaRequest
    sess
    OutRequestIQ
      { oriTo = Just addr
      , oriIqType = IQGet
      , oriChildren = [closedElement (pingName "ping")]
      }
    $ \resp -> handler $ case resp of
      Left e -> Left e
      Right _ -> Right ()

pingPlugin :: (MonadStream m) => XMPPPluginsRef m -> m ()
pingPlugin pluginsRef = do
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM PingPlugin iqHandlers
  addDiscoInfo pluginsRef PingPlugin
