{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Myself (
  myPresenceGet,
  myPresenceSlot,
  myPresenceSend,
  myPresencePlugin,
) where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import UnliftIO.IORef

import qualified Control.HandlerList as HandlerList
import Control.Slot (Slot)
import qualified Control.Slot as Slot
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

myPresencePHandler :: (MonadStream m) => MyPresenceSlot m -> MyPresenceState m -> PresenceHandler m
myPresencePHandler slot (MyPresenceState {..}) (ResourcePresence from pres)
  | fullBare from == fullBare (sessionAddress $ ssSession myPresenceSession) = do
      presences <- readIORef myPresenceRef
      case presenceUpdate (fullResource from) pres presences of
        Nothing -> return Nothing
        Just (presences', event) -> do
          atomicWriteIORef myPresenceRef presences'
          Slot.call slot event
          return $ Just ()
myPresencePHandler _ _ _ = return Nothing

myPresenceGet :: forall m. (MonadStream m) => XMPPPluginsRef m -> m MyselfPresenceMap
myPresenceGet pluginsRef = do
  MyPresenceState {..} <- getPluginsHook (Proxy :: Proxy (MyPresenceState m)) pluginsRef
  readIORef myPresenceRef

myPresenceSlot :: (MonadStream m) => XMPPPluginsRef m -> m (MyPresenceSlot m)
myPresenceSlot = getPluginsHook Proxy

myPresenceSend :: (MonadStream m) => XMPPPluginsRef m -> Maybe Presence -> m ()
myPresenceSend pluginsRef pres = void $ stanzaSend (pluginsSession pluginsRef) $ presenceStanza pres

myPresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
myPresencePlugin pluginsRef = do
  myPresenceRef <- newIORef M.empty
  let state = MyPresenceState {myPresenceSession = pluginsSession pluginsRef, ..}
  insertPluginsHook state pluginsRef
  slot <- Slot.new
  insertPluginsHook slot pluginsRef
  pHandlers <- presenceHandlers pluginsRef
  void $ HandlerList.add pHandlers (myPresencePHandler slot state)
