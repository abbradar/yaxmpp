{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Myself (
  myPresenceGet,
  myPresenceSlot,
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

type MyselfPresenceMap = Map XMPPResource Presence

type MyPresenceSlot m = Slot m (PresenceEvent XMPPResource)

data MyPresenceState m = MyPresenceState
  { myPresenceRef :: IORef MyselfPresenceMap
  , myPresenceSession :: StanzaSession m
  }

data MyPresencePlugin m = MyPresencePlugin
  { myPresencePluginSlot :: MyPresenceSlot m
  , myPresencePluginState :: MyPresenceState m
  }

instance (MonadStream m) => Handler m PresenceUpdate () (MyPresencePlugin m) where
  tryHandle (MyPresencePlugin {..}) (ResourcePresence from pres)
    | fullBare from == fullBare (sessionAddress $ ssSession myPresenceSession) = do
        presences <- readIORef myPresenceRef
        case presenceUpdate (fullResource from) pres presences of
          Nothing -> return Nothing
          Just (presences', event) -> do
            atomicWriteIORef myPresenceRef presences'
            Slot.call myPresencePluginSlot event
            return $ Just ()
   where
    MyPresenceState {..} = myPresencePluginState
  tryHandle _ _ = return Nothing

myPresenceGet :: forall m. (MonadStream m) => XMPPPluginsRef m -> m MyselfPresenceMap
myPresenceGet pluginsRef = do
  MyPresenceState {..} <- RegRef.lookupOrFailM (Proxy :: Proxy (MyPresenceState m)) $ pluginsHooksSet pluginsRef
  readIORef myPresenceRef

myPresenceSlot :: (MonadStream m) => XMPPPluginsRef m -> m (MyPresenceSlot m)
myPresenceSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

myPresenceSend :: (MonadStream m) => XMPPPluginsRef m -> Maybe Presence -> m ()
myPresenceSend pluginsRef pres = do
  stanza <- presenceStanza pluginsRef pres
  void $ stanzaSend (pluginsSession pluginsRef) stanza

myPresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
myPresencePlugin pluginsRef = do
  myPresenceRef <- newIORef M.empty
  let myPresencePluginState = MyPresenceState {myPresenceSession = pluginsSession pluginsRef, ..}
  myPresencePluginSlot <- Slot.new
  let plugin :: MyPresencePlugin m = MyPresencePlugin {..}
  RegRef.insertNewOrFailM myPresencePluginState $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM myPresencePluginSlot $ pluginsHooksSet pluginsRef
  pHandlers <- presenceHandlers pluginsRef
  HL.pushNewOrFailM plugin pHandlers
