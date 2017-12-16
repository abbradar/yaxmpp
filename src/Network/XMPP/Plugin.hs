module Network.XMPP.Plugin where

import Data.Text (Text)
import Control.Monad.Logger
import Text.InterpolatedString.Perl6 (qq)
import Text.XML
import Data.Default.Class

import Network.XMPP.Stream
import Network.XMPP.Stanza

type XMPPFeature = Text

type PluginInHandler m = InStanza -> m (Maybe (Maybe StanzaError))
type PluginRequestIQHandler m = InRequestIQ -> m (Maybe (Either StanzaError [Element]))

data XMPPPlugin m = XMPPPlugin { pluginInHandler :: PluginInHandler m
                               , pluginRequestIqHandler :: PluginRequestIQHandler m
                               }

instance Monad m => Default (XMPPPlugin m) where
  def = XMPPPlugin { pluginInHandler = \_ -> return Nothing
                   , pluginRequestIqHandler = \_ -> return Nothing
                   }

pluginsInHandler :: MonadStream m => [XMPPPlugin m] -> InHandler m
pluginsInHandler [] msg = do
  $(logWarn) [qq|Unhandled stanza: $msg|]
  -- Shouldn't reply to unknown messages/presences.
  return Nothing
pluginsInHandler ((XMPPPlugin {..}):plugins) msg = do
  res <- pluginInHandler msg
  case res of
    Nothing -> pluginsInHandler plugins msg
    Just r -> return r

pluginsRequestIqHandler :: MonadStream m => [XMPPPlugin m] -> RequestIQHandler m
pluginsRequestIqHandler [] iq = do
  $(logWarn) [qq|Unhandled request: $iq|]
  return $ Left $ serviceUnavailable "Unsupported request"
pluginsRequestIqHandler ((XMPPPlugin {..}):plugins) iq = do
  res <- pluginRequestIqHandler iq
  case res of
    Nothing -> pluginsRequestIqHandler plugins iq
    Just r -> return r
