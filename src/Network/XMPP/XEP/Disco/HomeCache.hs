{-# LANGUAGE Strict #-}

{- | Disco entity cache for the home server (the session's bare domain JID,
no node). The home entity is fetched at most once per session and shared.
-}
module Network.XMPP.XEP.Disco.HomeCache (
  HomeCachePlugin,
  getHomeCachePlugin,
  getHomeDiscoEntity,
  newHomeFeatureCheck,
  homeCachePlugin,
) where

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Session (sessionAddress)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco

data HomeCachePlugin m = HomeCachePlugin
  { hcpAddr :: XMPPAddress
  , hcpEntity :: AsyncMemo m (Either StanzaError DiscoEntity)
  }

instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (HomeCachePlugin m) where
  tryHandle (HomeCachePlugin {..}) (addr, Nothing, handler)
    | addr == hcpAddr = Just <$> AsyncMemo.get hcpEntity handler
  tryHandle _ _ = return Nothing

getHomeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (HomeCachePlugin m)
getHomeCachePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (HomeCachePlugin m)) $ pluginsHooksSet pluginsRef

-- | Get the cached home server disco entity, fetching on first call.
getHomeDiscoEntity :: (MonadStream m) => HomeCachePlugin m -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getHomeDiscoEntity (HomeCachePlugin {hcpEntity}) = AsyncMemo.get hcpEntity

{- | Build a memoized check of whether the home server advertises a disco feature.
The underlying entity fetch is already memoized, but wrapping the boolean result
means each consumer can cache its own answer instead of re-traversing the feature set.
Disco errors collapse to 'False' (treat the feature as unsupported).
-}
newHomeFeatureCheck :: (MonadStream m) => HomeCachePlugin m -> DiscoFeature -> m (AsyncMemo m Bool)
newHomeFeatureCheck hcp feat = AsyncMemo.new $ \cb ->
  getHomeDiscoEntity hcp $ \case
    Left _ -> cb False
    Right ent -> cb (feat `S.member` discoFeatures ent)

homeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
homeCachePlugin pluginsRef = do
  dp <- getDiscoPlugin pluginsRef
  let myAddr = sessionAddress $ ssSession $ discoPluginSession dp
      hcpAddr = XMPPAddress Nothing (bareDomain $ fullBare myAddr) Nothing
  hcpEntity <- AsyncMemo.new $ getDiscoEntityNoCache dp hcpAddr Nothing
  let plugin :: HomeCachePlugin m = HomeCachePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ discoPluginEntityCacheHandlers dp
