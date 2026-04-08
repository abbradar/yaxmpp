{-# LANGUAGE Strict #-}

module Network.XMPP.Plugin (
  XMPPFeature,
  PluginInHandler,
  PluginIQHandler,
  XMPPPluginsRef,
  insertPluginsHook,
  getPluginsHook,
  newXmppPlugins,
  pluginsSession,
  pluginsInHandlers,
  pluginsIQHandlers,
  pluginsSessionStep,
) where

import Control.HandlerList (HandlerList)
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.ClassBox (Unconstrained)
import Data.IORef
import Data.Proxy
import Data.Registry (Registry)
import qualified Data.Registry as Reg
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.XMPP.Stanza
import Network.XMPP.Stream

type XMPPFeature = Text

data XMPPPluginsRef m = XMPPPluginsRef
  { pluginsSession :: StanzaSession m
  , pluginsHooksSet :: IORef (Registry Unconstrained)
  , pluginsInHandlers' :: HandlerList m InStanza InResponse
  , pluginsIQHandlers' :: HandlerList m InRequestIQ RequestIQResponse
  }

type PluginInHandler m = InStanza -> m (Maybe InResponse)

type PluginIQHandler m = InRequestIQ -> m (Maybe RequestIQResponse)

newXmppPlugins :: forall m. (MonadStream m) => StanzaSession m -> m (XMPPPluginsRef m)
newXmppPlugins pluginsSession = do
  pluginsInHandlers' <- HL.new
  pluginsIQHandlers' <- HL.new
  let set =
        Reg.insert pluginsInHandlers' $
          Reg.insert pluginsIQHandlers' $
            Reg.empty
  pluginsHooksSet <- liftIO $ newIORef set
  return XMPPPluginsRef {..}

-- | Insert a value into the plugins set. Fails if one already exists for this type.
insertPluginsHook :: forall a m. (MonadStream m, Typeable a) => a -> XMPPPluginsRef m -> m ()
insertPluginsHook v (XMPPPluginsRef {..}) = do
  success <- liftIO $ atomicModifyIORef pluginsHooksSet $ \hooks ->
    case Reg.tryInsertNew v hooks of
      Nothing -> (hooks, False)
      Just hooks' -> (hooks', True)
  unless success $ error "insertPluginsHook: hook already exists"

-- | Get an existing hook from the plugins set. Fails if it doesn't exist.
getPluginsHook :: (MonadStream m, Typeable a) => Proxy a -> XMPPPluginsRef m -> m a
getPluginsHook k (XMPPPluginsRef {..}) = do
  hooks <- liftIO $ readIORef pluginsHooksSet
  case Reg.lookup k hooks of
    Just h -> return h
    Nothing -> error "getPluginsHook: hook does not exist; is the plugin initialized?"

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
