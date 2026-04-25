{-# LANGUAGE Strict #-}

{- | Version cache for the home server (the session's bare domain JID).
The home version is fetched at most once per session and shared.
-}
module Network.XMPP.XEP.Version.HomeCache (
  VersionHomeCachePlugin,
  getVersionHomeCachePlugin,
  getHomeVersion,
  versionHomeCachePlugin,
) where

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Session (sessionAddress)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Version

data VersionHomeCachePlugin m = VersionHomeCachePlugin
  { vhcpAddr :: XMPPAddress
  , vhcpInfo :: AsyncMemo m (Either StanzaError VersionInfo)
  }

instance (MonadStream m) => Handler m (XMPPAddress, Either StanzaError VersionInfo -> m ()) () (VersionHomeCachePlugin m) where
  tryHandle (VersionHomeCachePlugin {..}) (addr, handler)
    | addr == vhcpAddr = Just <$> AsyncMemo.get vhcpInfo handler
  tryHandle _ _ = return Nothing

getVersionHomeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (VersionHomeCachePlugin m)
getVersionHomeCachePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (VersionHomeCachePlugin m)) $ pluginsHooksSet pluginsRef

-- | Get the cached home server version, fetching on first call.
getHomeVersion :: (MonadStream m) => VersionHomeCachePlugin m -> (Either StanzaError VersionInfo -> m ()) -> m ()
getHomeVersion (VersionHomeCachePlugin {vhcpInfo}) = AsyncMemo.get vhcpInfo

versionHomeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
versionHomeCachePlugin pluginsRef = do
  vp <- getVersionPlugin pluginsRef
  let myAddr = sessionAddress $ ssSession $ versionPluginSession vp
      vhcpAddr = XMPPAddress Nothing (bareDomain $ fullBare myAddr) Nothing
  vhcpInfo <- AsyncMemo.new $ requestVersion (versionPluginSession vp) vhcpAddr
  let plugin :: VersionHomeCachePlugin m = VersionHomeCachePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ versionPluginCacheHandlers vp
