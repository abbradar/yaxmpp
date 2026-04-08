{-# LANGUAGE Strict #-}

module Network.XMPP.Plugin (
  XMPPFeature,
  PluginInHandler,
  PluginIQHandler,
  XMPPPluginsRef (..),
  newXmppPlugins,
  pluginsInHandlers,
  pluginsIQHandlers,
  pluginsSessionStep,
) where

import Control.HandlerList (HandlerList)
import qualified Control.HandlerList as HL
import Control.Monad.Logger
import Data.ClassBox (Unconstrained)
import Data.Registry.Mutable (RegistryRef)
import qualified Data.Registry.Mutable as RegRef
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.XMPP.Stanza
import Network.XMPP.Stream

type XMPPFeature = Text

data XMPPPluginsRef m = XMPPPluginsRef
  { pluginsSession :: StanzaSession m
  , pluginsHooksSet :: RegistryRef Unconstrained
  , pluginsServerFeatures :: RegistryRef Show
  , pluginsInHandlers' :: HandlerList m InStanza InResponse
  , pluginsIQHandlers' :: HandlerList m InRequestIQ RequestIQResponse
  }

type PluginInHandler m = InStanza -> m (Maybe InResponse)

type PluginIQHandler m = InRequestIQ -> m (Maybe RequestIQResponse)

newXmppPlugins :: forall m. (MonadStream m) => StanzaSession m -> m (XMPPPluginsRef m)
newXmppPlugins pluginsSession = do
  pluginsInHandlers' <- HL.new
  pluginsIQHandlers' <- HL.new
  pluginsHooksSet <- RegRef.new
  RegRef.insert pluginsInHandlers' pluginsHooksSet
  RegRef.insert pluginsIQHandlers' pluginsHooksSet
  pluginsServerFeatures <- RegRef.new
  return XMPPPluginsRef {..}

pluginsInHandlers :: (MonadStream m) => XMPPPluginsRef m -> m (HandlerList m InStanza InResponse)
pluginsInHandlers = return . pluginsInHandlers'

pluginsIQHandlers :: (MonadStream m) => XMPPPluginsRef m -> m (HandlerList m InRequestIQ RequestIQResponse)
pluginsIQHandlers = return . pluginsIQHandlers'

pluginsInHandler :: (MonadStream m) => XMPPPluginsRef m -> InHandler m
pluginsInHandler (XMPPPluginsRef {..}) msg = do
  mr <- HL.call pluginsInHandlers' msg
  case mr of
    Nothing -> do
      $(logWarn) [i|Unhandled stanza: #{msg}|]
      return InSilent
    Just r -> return r

pluginsIQHandler :: (MonadStream m) => XMPPPluginsRef m -> IQHandler m
pluginsIQHandler (XMPPPluginsRef {..}) iq = do
  mr <- HL.call pluginsIQHandlers' iq
  case mr of
    Nothing -> return $ IQError $ serviceUnavailable "Unsupported request"
    Just r -> return r

pluginsSessionStep :: (MonadStream m) => XMPPPluginsRef m -> m ()
pluginsSessionStep ref = stanzaSessionStep (pluginsSession ref) (pluginsInHandler ref) (pluginsIQHandler ref)
