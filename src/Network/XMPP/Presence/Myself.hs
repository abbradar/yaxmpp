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
import Data.IORef.Lifted

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.Presence

data MyPresenceRef m = MyPresenceRef { myPresenceHandler :: Handler m (PresenceEvent XMPPResource)
                                     , myPresence :: IORef (Map XMPPResource Presence)
                                     , myPresenceSession :: StanzaSession m
                                     , myBareAddress :: BareJID
                                     }

myPresencePHandler :: MonadStream m => MyPresenceRef m -> PresenceHandler m
myPresencePHandler (MyPresenceRef {..}) (FullJID {..}) pres
  | myBareAddress == fullBare = do
      presences <- readIORef myPresence
      case presenceUpdate fullResource pres presences of
        Nothing -> return False
        Just (presences', event) -> do
          writeIORef myPresence presences'
          Handler.call myPresenceHandler event
          return True
myPresencePHandler _ _ _ = return False

myPresenceGet :: MonadStream m => MyPresenceRef m -> m (Map XMPPResource Presence)
myPresenceGet = readIORef . myPresence

myPresenceSetHandler :: MonadStream m => MyPresenceRef m -> (PresenceEvent XMPPResource -> m ()) -> m ()
myPresenceSetHandler (MyPresenceRef {..}) = Handler.set myPresenceHandler

myPresenceSend :: MonadStream m => MyPresenceRef m -> Maybe Presence -> m ()
myPresenceSend (MyPresenceRef {..}) pres = void $ stanzaSend myPresenceSession $ presenceStanza pres

myPresencePlugin :: MonadStream m => StanzaSession m -> m (PresenceHandler m, MyPresenceRef m)
myPresencePlugin myPresenceSession = do
  myPresence <- newIORef M.empty
  myPresenceHandler <- Handler.new
  let myBareAddress = fullBare $ sessionAddress $ ssSession myPresenceSession
      pref = MyPresenceRef {..}
      phandler = myPresencePHandler pref
  return (phandler, pref)
