module Network.XMPP.Plugin where

import Control.Monad.Logger
import Text.InterpolatedString.Perl6 (qq)
import Text.XML

import Network.XMPP.Session
import Network.XMPP.Stanza

data XMPPPlugin m = XMPPPlugin { pluginInHandler ::  InStanza -> m (Maybe (Maybe StanzaError))
                               , pluginRequestIqHandler ::  InRequestIQ -> m (Maybe (Either StanzaError [Element]))
                               }

pluginsInHandler :: MonadSession m => [XMPPPlugin m] -> InHandler m
pluginsInHandler [] msg = do
  $(logWarn) [qq|Unhandled stanza: $msg|]
  -- Shouldn't reply to unknown messages/presences.
  return Nothing
pluginsInHandler ((XMPPPlugin {..}):plugins) msg = do
  res <- pluginInHandler msg
  case res of
    Nothing -> pluginsInHandler plugins msg
    Just r -> return r

pluginsRequestIqHandler :: MonadSession m => [XMPPPlugin m] -> RequestIQHandler m
pluginsRequestIqHandler [] iq = do
  $(logWarn) [qq|Unhandled request: $iq|]
  return $ Left $ featureNotImplemented "Unsupported request"
pluginsRequestIqHandler ((XMPPPlugin {..}):plugins) iq = do
  res <- pluginRequestIqHandler iq
  case res of
    Nothing -> pluginsRequestIqHandler plugins iq
    Just r -> return r
