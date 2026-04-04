{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Myself
  ( MyPresenceRef
  , myPresenceGet
  , myPresenceSlot
  , myPresenceSend
  , myPresencePlugin
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import UnliftIO.IORef

import Control.Slot (Slot, SlotRef)
import qualified Control.Slot as Slot
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.Presence

type MyselfPresenceMap = Map XMPPResource Presence

data MyPresenceRef m = MyPresenceRef { myPresenceHandler :: Slot m (PresenceEvent XMPPResource)
                                     , myPresence :: IORef MyselfPresenceMap
                                     , myPresenceSession :: StanzaSession m
                                     }

myPresencePHandler :: MonadStream m => MyPresenceRef m -> PresenceHandler m
myPresencePHandler (MyPresenceRef {..}) (from, pres)
  | fullBare from == fullBare (sessionAddress $ ssSession myPresenceSession) = do
      presences <- readIORef myPresence
      case presenceUpdate (fullResource from) pres presences of
        Nothing -> return Nothing
        Just (presences', event) -> do
          atomicWriteIORef myPresence presences'
          Slot.call myPresenceHandler event
          return $ Just ()
myPresencePHandler _ _ = return Nothing

myPresenceGet :: MonadStream m => MyPresenceRef m -> m MyselfPresenceMap
myPresenceGet = readIORef . myPresence

myPresenceSlot :: MyPresenceRef m -> SlotRef m (PresenceEvent XMPPResource)
myPresenceSlot (MyPresenceRef {..}) = Slot.ref myPresenceHandler

myPresenceSend :: MonadStream m => MyPresenceRef m -> Maybe Presence -> m ()
myPresenceSend (MyPresenceRef {..}) pres = void $ stanzaSend myPresenceSession $ presenceStanza pres

myPresencePlugin :: MonadStream m => StanzaSession m -> m (PresenceHandler m, MyPresenceRef m)
myPresencePlugin myPresenceSession = do
  myPresence <- newIORef M.empty
  myPresenceHandler <- Slot.new
  let pref = MyPresenceRef {..}
      phandler = myPresencePHandler pref
  return (phandler, pref)
