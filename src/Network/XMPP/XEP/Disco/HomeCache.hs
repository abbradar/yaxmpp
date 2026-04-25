{-# LANGUAGE Strict #-}

{- | Disco entity cache for the session's home (account) bare JID, no node.
The home entity is fetched at most once per session and shared.
-}
module Network.XMPP.XEP.Disco.HomeCache (
  HomeCachePlugin,
  getHomeCachePlugin,
  homeCachePlugin,
) where

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco

data HomeCachePlugin m = HomeCachePlugin
  { hcpAddr :: BareJID
  , hcpEntity :: AsyncMemo m (Either StanzaError DiscoEntity)
  }

instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (HomeCachePlugin m) where
  tryHandle (HomeCachePlugin {..}) (addr, Nothing, handler)
    | addr == toXMPPAddress hcpAddr = Just <$> AsyncMemo.get hcpEntity handler
  tryHandle _ _ = return Nothing

getHomeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (HomeCachePlugin m)
getHomeCachePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (HomeCachePlugin m)) $ pluginsHooksSet pluginsRef

homeCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
homeCachePlugin pluginsRef = do
  dp <- getDiscoPlugin pluginsRef
  let hcpAddr = ssServer $ discoPluginSession dp
  hcpEntity <- AsyncMemo.new $ getDiscoEntityNoCache dp hcpAddr Nothing
  let plugin :: HomeCachePlugin m = HomeCachePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ discoPluginEntityCacheHandlers dp
