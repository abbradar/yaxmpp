{-# LANGUAGE Strict #-}

{- | Disco entity cache keyed on bare JIDs of joined MUC rooms. On the
'MUCJoinedRoom' broadcast, attaches a fresh 'DiscoNodeCache' to the room's
mutable registry; on disco lookup of the room's bare JID with any node,
serves through that cache. The cache lives as long as the room is joined.
-}
module Network.XMPP.XEP.Disco.MUCCache (
  mucDiscoCachePlugin,
) where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (SlotSignal (..))
import qualified Control.Slot as Slot
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import qualified Network.XMPP.XEP.Disco.NodeCache as DiscoNodeCache
import Network.XMPP.XEP.MUC

data MUCDiscoCachePlugin m = MUCDiscoCachePlugin
  { mdcpDisco :: DiscoPlugin m
  , mdcpMUC :: MUCPlugin m
  }

{- | Listens to the global MUC slot:
* On 'MUCJoinedRoom' attaches a fresh 'DiscoNodeCache' to the room's registry.
* On 'RoomConfigChanged' invalidates the cache.
-}
instance (MonadStream m) => SlotSignal m (MUCEvent m) (MUCDiscoCachePlugin m) where
  emitSignal _ (MUCJoinedRoom _ ref) = do
    let state = mucState ref
    existing <- RegRef.lookup (Proxy :: Proxy (DiscoNodeCache.DiscoNodeCache m)) state
    case existing of
      Just _ -> return ()
      Nothing -> do
        cache <- DiscoNodeCache.new
        RegRef.insert (cache :: DiscoNodeCache.DiscoNodeCache m) state
  emitSignal _ (MUCRoomEvent _ ref (RoomConfigChanged _)) = do
    mCache <- RegRef.lookup (Proxy :: Proxy (DiscoNodeCache.DiscoNodeCache m)) (mucState ref)
    forM_ mCache DiscoNodeCache.clear
  emitSignal _ _ = return ()

instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (MUCDiscoCachePlugin m) where
  tryHandle (MUCDiscoCachePlugin {..}) (addr, node, handler)
    | Just bare <- bareJidGet addr = do
        mRef <- lookupMUCRoom mdcpMUC bare
        case mRef of
          Just ref -> do
            mCache <- RegRef.lookup (Proxy :: Proxy (DiscoNodeCache.DiscoNodeCache m)) (mucState ref)
            case mCache of
              Just cache -> Just <$> DiscoNodeCache.get mdcpDisco cache addr node handler
              -- There may be a brief race condition.
              Nothing -> return Nothing
          Nothing -> return Nothing
  tryHandle _ _ = return Nothing

mucDiscoCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
mucDiscoCachePlugin pluginsRef = do
  mdcpDisco <- getDiscoPlugin pluginsRef
  mdcpMUC <- getMUCPlugin pluginsRef
  let plugin :: MUCDiscoCachePlugin m = MUCDiscoCachePlugin {..}
  HL.pushNewOrFailM plugin $ discoPluginEntityCacheHandlers mdcpDisco
  Slot.pushNewOrFailM plugin $ mucPluginSlot mdcpMUC
