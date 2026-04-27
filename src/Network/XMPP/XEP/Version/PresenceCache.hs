{-# LANGUAGE Strict #-}

{- | Version cache keyed on full JIDs known via active presence. On the first
presence event for a resource, attaches a lazy 'AsyncMemo' to its
mutable registry; on version lookup of any full JID with an active
presence, serves through that memo. The cache lives as long as the resource
is online.
-}
module Network.XMPP.XEP.Version.PresenceCache (
  versionPresenceCachePlugin,
) where

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (SlotSignal (..))
import qualified Control.Slot as Slot
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Version

newtype LazyVersion m = LazyVersion (AsyncMemo m (Either StanzaError VersionInfo))

data VersionPresenceCachePlugin m = VersionPresenceCachePlugin
  { vpcpVersion :: VersionPlugin m
  , vpcpPresence :: PresencePlugin m
  }

instance (MonadStream m) => SlotSignal m (PresenceUpdate m) (VersionPresenceCachePlugin m) where
  emitSignal (VersionPresenceCachePlugin {vpcpVersion}) (ResourcePresence faddr (ResourceAvailable (presenceState -> state))) = do
    existing <- RegRef.lookup (Proxy :: Proxy (LazyVersion m)) state
    case existing of
      Just _ -> return ()
      Nothing -> do
        lazy <- AsyncMemo.new $ requestVersion (versionPluginSession vpcpVersion) faddr
        RegRef.insert (LazyVersion lazy :: LazyVersion m) state
  emitSignal _ _ = return ()

instance (MonadStream m) => Handler m (XMPPAddress, Either StanzaError VersionInfo -> m ()) () (VersionPresenceCachePlugin m) where
  tryHandle (VersionPresenceCachePlugin {vpcpPresence}) (addr, handler)
    | Just full <- fullJidGet addr = do
        presences <- getAllPresences vpcpPresence
        case M.lookup full presences of
          Just (presenceState -> state) -> do
            mLazy <- RegRef.lookup (Proxy :: Proxy (LazyVersion m)) state
            case mLazy of
              Just (LazyVersion lazy) -> Just <$> AsyncMemo.get lazy handler
              Nothing -> fail "LazyVersion not found in Presence"
          Nothing -> return Nothing
  tryHandle _ _ = return Nothing

versionPresenceCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
versionPresenceCachePlugin pluginsRef = do
  vpcpVersion <- getVersionPlugin pluginsRef
  vpcpPresence <- getPresencePlugin pluginsRef
  let plugin :: VersionPresenceCachePlugin m = VersionPresenceCachePlugin {..}
  HL.pushNewOrFailM plugin $ versionPluginCacheHandlers vpcpVersion
  Slot.pushNewOrFailM plugin $ presencePluginSlot vpcpPresence
