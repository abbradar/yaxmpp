{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Myself (
  MyPresenceSlot,
  MyPresencePlugin,
  myPresencePluginSlot,
  getMyPresencePlugin,
  myPresenceGet,
  myPresenceSend,
  myPresencePlugin,
) where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import UnliftIO.IORef

import qualified Data.Registry.Mutable as RegRef
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Stream

type MyselfPresenceMap m = Map XMPPResource (PresenceRef m)

type MyPresenceSlot m = Slot m (PresenceEvent m XMPPResource)

data MyPresencePlugin m = MyPresencePlugin
  { myPresencePluginSession :: StanzaSession m
  , myPresencePluginPresencePlugin :: PresencePlugin m
  , myPresencePluginSlot :: MyPresenceSlot m
  , myPresencePluginRef :: IORef (MyselfPresenceMap m)
  }

instance (MonadStream m) => Handler m (PresenceUpdate m) () (MyPresencePlugin m) where
  tryHandle (MyPresencePlugin {..}) (ResourcePresence from pres)
    | fullBare from == fullBare (sessionAddress $ ssSession myPresencePluginSession) = do
        presences <- readIORef myPresencePluginRef
        case presenceUpdate (fullResource from) pres presences of
          Nothing -> return Nothing
          Just (presences', event) -> do
            atomicWriteIORef myPresencePluginRef presences'
            Slot.call myPresencePluginSlot event
            return $ Just ()
  tryHandle _ _ = return Nothing

getMyPresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (MyPresencePlugin m)
getMyPresencePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (MyPresencePlugin m)) $ pluginsHooksSet pluginsRef

myPresenceGet :: (MonadStream m) => MyPresencePlugin m -> m (MyselfPresenceMap m)
myPresenceGet MyPresencePlugin {myPresencePluginRef} = readIORef myPresencePluginRef

myPresenceSend :: (MonadStream m) => MyPresencePlugin m -> Maybe Presence -> m ()
myPresenceSend MyPresencePlugin {myPresencePluginSession, myPresencePluginPresencePlugin} pres = do
  stanza <- presenceStanza myPresencePluginPresencePlugin pres
  void $ stanzaSend myPresencePluginSession stanza

myPresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
myPresencePlugin pluginsRef = do
  myPresencePluginRef <- newIORef M.empty
  myPresencePluginSlot <- Slot.new
  myPresencePluginPresencePlugin <- getPresencePlugin pluginsRef
  let myPresencePluginSession = pluginsSession pluginsRef
      plugin :: MyPresencePlugin m = MyPresencePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin (presencePluginHandlers myPresencePluginPresencePlugin)
