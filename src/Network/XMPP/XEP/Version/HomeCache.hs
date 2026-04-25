{-# LANGUAGE Strict #-}

{- | Version cache for the session's home (account) bare JID.
The home version is fetched at most once per session and shared.
-}
module Network.XMPP.XEP.Version.HomeCache (
  versionHomeCachePlugin,
) where

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Version

data VersionHomeCacheHandler m = VersionHomeCacheHandler
  { vhcAddr :: XMPPAddress
  , vhcInfo :: AsyncMemo m (Either StanzaError VersionInfo)
  }

instance (MonadStream m) => Handler m (XMPPAddress, Either StanzaError VersionInfo -> m ()) () (VersionHomeCacheHandler m) where
  tryHandle (VersionHomeCacheHandler {..}) (addr, handler)
    | addr == vhcAddr = Just <$> AsyncMemo.get vhcInfo handler
  tryHandle _ _ = return Nothing

versionHomeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
versionHomeCachePlugin pluginsRef = do
  vp <- getVersionPlugin pluginsRef
  let vhcAddr = toXMPPAddress $ ssServer $ versionPluginSession vp
  vhcInfo <- AsyncMemo.new $ requestVersion (versionPluginSession vp) vhcAddr
  HL.pushNewOrFailM VersionHomeCacheHandler {..} $ versionPluginCacheHandlers vp
