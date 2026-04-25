{-# LANGUAGE Strict #-}

{- | Disco entity cache keyed on full JIDs known via active presence. Decorates
each incoming presence with a lazy disco entity slot; on disco lookup of a
full JID with no node, fires the slot (which fetches once and memoizes for
the lifetime of that presence).
-}
module Network.XMPP.XEP.Disco.PresenceCache (
  PresenceCachePlugin,
  presenceCachePlugin,
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
import Network.XMPP.XEP.Disco

-- | Lazy disco entity cache stored in each presence's extended registry.
newtype LazyDiscoEntity m = LazyDiscoEntity (AsyncMemo m (Either StanzaError DiscoEntity))

instance Show (LazyDiscoEntity m) where
  show _ = "LazyDiscoEntity"

data PresenceCachePlugin m = PresenceCachePlugin
  { pcpDisco :: DiscoPlugin m
  , pcpPresence :: PresencePlugin m
  }

instance (MonadStream m) => Codec m FullJID Presence (PresenceCachePlugin m) where
  codecDecode (PresenceCachePlugin {pcpDisco}) faddr pres = do
    let addr = fullJidAddress faddr
    lazy <- AsyncMemo.new $ getDiscoEntityNoCache pcpDisco addr Nothing
    let lde = LazyDiscoEntity lazy :: LazyDiscoEntity m
    return $ pres {presenceExtended = Reg.insert lde (presenceExtended pres)}
  codecEncode _ _ pres =
    return $ pres {presenceExtended = Reg.delete (Proxy :: Proxy (LazyDiscoEntity m)) (presenceExtended pres)}

instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (PresenceCachePlugin m) where
  tryHandle (PresenceCachePlugin {pcpPresence}) (addr, Nothing, handler)
    | Just full <- fullJidGet addr = do
        presences <- getAllPresences pcpPresence
        case M.lookup full presences of
          Just pres
            | Just (LazyDiscoEntity lazy) <- Reg.lookup (Proxy :: Proxy (LazyDiscoEntity m)) (presenceExtended pres) ->
                Just <$> AsyncMemo.get lazy handler
          _ -> return Nothing
  tryHandle _ _ = return Nothing

presenceCachePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
presenceCachePlugin pluginsRef = do
  pcpDisco <- getDiscoPlugin pluginsRef
  pcpPresence <- getPresencePlugin pluginsRef
  let plugin :: PresenceCachePlugin m = PresenceCachePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ discoPluginEntityCacheHandlers pcpDisco
  Codec.pushNewOrFailM plugin $ presencePluginCodecs pcpPresence
