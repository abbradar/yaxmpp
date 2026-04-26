{-# LANGUAGE Strict #-}

{- | Disco entity cache keyed on full JIDs known via active presence. On the
first presence event for a resource, attaches a 'DiscoNodeCache' to its
mutable registry; on disco lookup of any full JID with any node,
serves through that cache. The cache lives as long as the resource is
online.
-}
module Network.XMPP.XEP.Disco.PresenceCache (
  presenceCachePlugin,
) where

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
import Network.XMPP.XEP.Disco
import qualified Network.XMPP.XEP.Disco.NodeCache as DiscoNodeCache

data PresenceCachePlugin m = PresenceCachePlugin
  { pcpDisco :: DiscoPlugin m
  , pcpPresence :: PresencePlugin m
  }

-- | On every available-presence event, ensure the resource has a 'DiscoNodeCache' in its mutable registry.
instance (MonadStream m) => SlotSignal m PresenceUpdate (PresenceCachePlugin m) where
  emitSignal _ (ResourcePresence _ (ResourceAvailable PresenceRef {presenceState})) = do
    existing <- RegRef.lookup (Proxy :: Proxy (DiscoNodeCache.DiscoNodeCache m)) presenceState
    case existing of
      Just _ -> return ()
      Nothing -> do
        cache <- DiscoNodeCache.new
        RegRef.insert (cache :: DiscoNodeCache.DiscoNodeCache m) presenceState
  emitSignal _ _ = return ()

instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (PresenceCachePlugin m) where
  tryHandle (PresenceCachePlugin {..}) (addr, node, handler)
    | Just full <- fullJidGet addr = do
        presences <- getAllPresences pcpPresence
        case M.lookup full presences of
          Just PresenceRef {presenceState} -> do
            mCache <- RegRef.lookup (Proxy :: Proxy (DiscoNodeCache.DiscoNodeCache m)) presenceState
            case mCache of
              Just cache -> Just <$> DiscoNodeCache.get pcpDisco cache addr node handler
              Nothing -> fail "DiscoNodeCache not found in Presence"
          Nothing -> return Nothing
  tryHandle _ _ = return Nothing

presenceCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
presenceCachePlugin pluginsRef = do
  pcpDisco <- getDiscoPlugin pluginsRef
  pcpPresence <- getPresencePlugin pluginsRef
  let plugin :: PresenceCachePlugin m = PresenceCachePlugin {..}
  HL.pushNewOrFailM plugin $ discoPluginEntityCacheHandlers pcpDisco
  Slot.pushNewOrFailM plugin $ presencePluginSlot pcpPresence
