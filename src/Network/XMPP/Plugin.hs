{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Strict #-}

module Network.XMPP.Plugin (
  XMPPFeature,
  PluginInHandler,
  PluginIQHandler,
  XMPPPersistentCache (..),
  XMPPPluginsRef (..),
  newXmppPlugins,
  pluginsInHandlers,
  pluginsIQHandlers,
  pluginsSessionStep,
  pluginsOldCacheFor,
  registerCacheGetter,
  getCache,
) where

import Control.HandlerList (HandlerList)
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Monad.Logger
import qualified Data.Aeson as JSON
import Data.ClassBox (ClassBox (..), Unconstrained)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Registry.Mutable (RegistryRef)
import qualified Data.Registry.Mutable as RegRef
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import UnliftIO.IORef

type XMPPFeature = Text

class XMPPPersistentCache m a | a -> m where
  cacheKey :: Proxy a -> Text
  cacheGet :: a -> m JSON.Value

data XMPPPluginsRef m = XMPPPluginsRef
  { pluginsSession :: StanzaSession m
  , pluginsHooksSet :: RegistryRef Unconstrained
  , pluginsInHandlers' :: HandlerList m InStanza InResponse
  , pluginsIQHandlers' :: HandlerList m InRequestIQ RequestIQResponse
  , pluginsOldCache :: Map Text JSON.Value
  , pluginsCacheGetters :: IORef (Map Text (ClassBox (XMPPPersistentCache m)))
  }

type PluginInHandler m = InStanza -> m (Maybe InResponse)

type PluginIQHandler m = InRequestIQ -> m (Maybe RequestIQResponse)

newXmppPlugins :: forall m. (MonadStream m) => StanzaSession m -> Maybe JSON.Value -> m (XMPPPluginsRef m)
newXmppPlugins pluginsSession oldCache = do
  let pluginsOldCache = case JSON.fromJSON <$> oldCache of
        Just (JSON.Success m) -> m
        _ -> M.empty
  pluginsInHandlers' <- HL.new
  pluginsIQHandlers' <- HL.new
  pluginsHooksSet <- RegRef.new
  RegRef.insert pluginsInHandlers' pluginsHooksSet
  RegRef.insert pluginsIQHandlers' pluginsHooksSet
  pluginsCacheGetters <- newIORef M.empty
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
pluginsSessionStep ref =
  stanzaSessionStep
    (pluginsSession ref)
    SessionHooks
      { hookInHandler = pluginsInHandler ref
      , hookIQHandler = pluginsIQHandler ref
      , hookOnReconnect = $(logInfo) "Session (re)connected"
      }

pluginsOldCacheFor :: forall a m. (XMPPPersistentCache m a) => XMPPPluginsRef m -> Proxy a -> Maybe JSON.Value
pluginsOldCacheFor ref _ = M.lookup (cacheKey (Proxy :: Proxy a)) $ pluginsOldCache ref

registerCacheGetter :: forall a m. (MonadStream m, XMPPPersistentCache m a) => XMPPPluginsRef m -> a -> m ()
registerCacheGetter ref a = do
  let key = cacheKey (Proxy :: Proxy a)
  success <- atomicModifyIORef' (pluginsCacheGetters ref) $ \m ->
    if M.member key m
      then (m, False)
      else (M.insert key (ClassBox a) m, True)
  unless success $ fail [i|registerCacheGetter: duplicate cache key: #{key}|]

getCache :: (MonadStream m) => XMPPPluginsRef m -> m JSON.Value
getCache ref = do
  getters <- readIORef (pluginsCacheGetters ref)
  values <- mapM (\(ClassBox a) -> cacheGet a) getters
  return $ JSON.toJSON $ M.filter (/= JSON.Null) values
