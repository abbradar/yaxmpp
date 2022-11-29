{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Myself
  ( MyPresenceRef
  , myPresenceGet
  , myPresenceSetHandler
  , myPresenceSend
  , myPresencePlugin
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import UnliftIO.IORef

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.Presence

type MyselfPresenceMap = Map XMPPResource Presence

data MyPresenceRef m = MyPresenceRef { myPresenceHandler :: Handler m (PresenceEvent XMPPResource)
                                     , myPresence :: IORef MyselfPresenceMap
                                     , myPresenceSession :: StanzaSession m
                                     }

myPresencePHandler :: MonadStream m => MyPresenceRef m -> PresenceHandler m
myPresencePHandler (MyPresenceRef {..}) from pres
  | fullBare from == fullBare (sessionAddress $ ssSession myPresenceSession) = do
      presences <- readIORef myPresence
      case presenceUpdate (fullResource from) pres presences of
        Nothing -> return False
        Just (presences', event) -> do
          atomicWriteIORef myPresence presences'
          Handler.call myPresenceHandler event
          return True
myPresencePHandler _ _ _ = return False

myPresenceGet :: MonadStream m => MyPresenceRef m -> m MyselfPresenceMap
myPresenceGet = readIORef . myPresence

myPresenceSetHandler :: MonadStream m => MyPresenceRef m -> (PresenceEvent XMPPResource -> m ()) -> m ()
myPresenceSetHandler (MyPresenceRef {..}) = Handler.set myPresenceHandler

myPresenceSend :: MonadStream m => MyPresenceRef m -> Maybe Presence -> m ()
myPresenceSend (MyPresenceRef {..}) pres = void $ stanzaSend myPresenceSession $ presenceStanza pres

myPresencePlugin :: MonadStream m => StanzaSession m -> m (PresenceHandler m, MyPresenceRef m)
myPresencePlugin myPresenceSession = do
  myPresence <- newIORef M.empty
  myPresenceHandler <- Handler.new
  let pref = MyPresenceRef {..}
      phandler = myPresencePHandler pref
  return (phandler, pref)
