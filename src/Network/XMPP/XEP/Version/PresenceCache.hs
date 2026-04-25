{-# LANGUAGE Strict #-}

{- | Version cache keyed on full JIDs known via active presence. Decorates
each incoming presence with a lazy version slot; on version lookup of a
full JID with an active presence, fires the slot (which fetches once and
memoizes for the lifetime of that presence).
-}
module Network.XMPP.XEP.Version.PresenceCache (
  VersionPresenceCachePlugin,
  versionPresenceCachePlugin,
) where

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef

import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Version

-- | Lazy version cache stored in each presence's extended registry.
newtype LazyVersion m = LazyVersion (AsyncMemo m (Either StanzaError VersionInfo))

instance Show (LazyVersion m) where
  show _ = "LazyVersion"

data VersionPresenceCachePlugin m = VersionPresenceCachePlugin
  { vpcpVersion :: VersionPlugin m
  , vpcpPresence :: PresencePlugin m
  }

instance (MonadStream m) => Codec m FullJID Presence (VersionPresenceCachePlugin m) where
  codecDecode (VersionPresenceCachePlugin {vpcpVersion}) faddr pres = do
    let addr = fullJidAddress faddr
    lazy <- AsyncMemo.new $ requestVersion (versionPluginSession vpcpVersion) addr
    let lv = LazyVersion lazy :: LazyVersion m
    return $ pres {presenceExtended = Reg.insert lv (presenceExtended pres)}
  codecEncode _ _ pres =
    return $ pres {presenceExtended = Reg.delete (Proxy :: Proxy (LazyVersion m)) (presenceExtended pres)}

instance (MonadStream m) => Handler m (XMPPAddress, Either StanzaError VersionInfo -> m ()) () (VersionPresenceCachePlugin m) where
  tryHandle (VersionPresenceCachePlugin {vpcpPresence}) (addr, handler)
    | Just full <- fullJidGet addr = do
        presences <- getAllPresences vpcpPresence
        case M.lookup full presences of
          Just pres
            | Just (LazyVersion lazy) <- Reg.lookup (Proxy :: Proxy (LazyVersion m)) (presenceExtended pres) ->
                Just <$> AsyncMemo.get lazy handler
          _ -> return Nothing
  tryHandle _ _ = return Nothing

versionPresenceCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
versionPresenceCachePlugin pluginsRef = do
  vpcpVersion <- getVersionPlugin pluginsRef
  vpcpPresence <- getPresencePlugin pluginsRef
  let plugin :: VersionPresenceCachePlugin m = VersionPresenceCachePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ versionPluginCacheHandlers vpcpVersion
  Codec.pushNewOrFailM plugin $ presencePluginCodecs vpcpPresence
