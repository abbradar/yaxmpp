{-# LANGUAGE Strict #-}

module Network.XMPP.Plugin
  ( XMPPFeature
  , PluginInHandler
  , PluginIQHandler
  , XMPPPluginsRef
  , newXmppPlugins
  , pluginsSession
  , pluginInHandlers
  , pluginIQHandlers
  , pluginsSessionStep
  ) where

import Data.Text (Text)
import Control.Monad.Logger
import Data.String.Interpolate (i)

import Control.HandlerList (HandlerList, HandlerListRef)
import qualified Control.HandlerList as HandlerList
import Network.XMPP.Stream
import Network.XMPP.Stanza

type XMPPFeature = Text

type PluginInHandler m = InStanza -> m (Maybe InResponse)
type PluginIQHandler m = InRequestIQ -> m (Maybe RequestIQResponse)

data XMPPPluginsRef m = XMPPPluginsRef { pluginInHandlersI :: HandlerList m InStanza InResponse
                                       , pluginIQHandlersI :: HandlerList m InRequestIQ RequestIQResponse
                                       , pluginsSession :: StanzaSession m
                                       }

newXmppPlugins :: MonadStream m => StanzaSession m -> m (XMPPPluginsRef m)
newXmppPlugins pluginsSession = do
  pluginInHandlersI <- HandlerList.new
  pluginIQHandlersI <- HandlerList.new
  return $ XMPPPluginsRef {..}

pluginInHandlers :: XMPPPluginsRef m -> HandlerListRef m InStanza InResponse
pluginInHandlers ref = HandlerList.ref $ pluginInHandlersI ref

pluginIQHandlers :: XMPPPluginsRef m -> HandlerListRef m InRequestIQ RequestIQResponse
pluginIQHandlers ref = HandlerList.ref $ pluginIQHandlersI ref

pluginsInHandler :: MonadStream m => XMPPPluginsRef m -> InHandler m
pluginsInHandler (XMPPPluginsRef {..}) msg = do
  mr <- HandlerList.call pluginInHandlersI msg
  case mr of
    Nothing -> do
      $(logWarn) [i|Unhandled stanza: #{msg}|]
      -- Shouldn't reply to unknown messages/presences.
      return InSilent
    Just r -> return r

pluginsIQHandler :: MonadStream m => XMPPPluginsRef m -> IQHandler m
pluginsIQHandler (XMPPPluginsRef {..}) iq = do
    mr <- HandlerList.call pluginIQHandlersI iq
    case mr of
      Nothing -> return $ IQError $ serviceUnavailable "Unsupported request"
      Just r -> return r

pluginsSessionStep :: MonadStream m => XMPPPluginsRef m -> m ()
pluginsSessionStep ref = stanzaSessionStep (pluginsSession ref) (pluginsInHandler ref) (pluginsIQHandler ref)
