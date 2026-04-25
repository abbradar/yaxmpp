{-# LANGUAGE Strict #-}

{- | Disco entity cache for the session's home: both the home server (the
session's bare domain JID, no node) and the user's bare JID (used e.g. for
XEP-0313 MAM lookups). All disco nodes for these addresses are cached lazily
for the session.
-}
module Network.XMPP.XEP.Disco.HomeCache (
  homeCachePlugin,
) where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.Disco.Cache.NodeCache

data HomeCacheHandler m = HomeCacheHandler
  { hchDisco :: DiscoPlugin m
  , hchAddr :: XMPPAddress
  , hchCache :: DiscoNodeCache m
  }

instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (HomeCacheHandler m) where
  tryHandle (HomeCacheHandler {..}) (addr, node, handler)
    | addr == hchAddr = Just <$> getDiscoNodeCache hchDisco hchCache addr node handler
  tryHandle _ _ = return Nothing

homeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
homeCachePlugin pluginsRef = do
  hchDisco <- getDiscoPlugin pluginsRef
  let bare = ssServer $ discoPluginSession hchDisco
      server = bareDomain bare
  serverCache <- newDiscoNodeCache
  bareCache <- newDiscoNodeCache
  HL.push HomeCacheHandler {hchAddr = toXMPPAddress server, hchCache = serverCache, ..} $ discoPluginEntityCacheHandlers hchDisco
  HL.push HomeCacheHandler {hchAddr = toXMPPAddress bare, hchCache = bareCache, ..} $ discoPluginEntityCacheHandlers hchDisco
