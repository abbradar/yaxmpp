module Network.XMPP.Presence.Myself
  ( MyPresenceRef
  , myPresenceGet
  , myPresenceSetHandler
  , myPresenceSend
  , myPresencePlugin
  ) where

import Data.Maybe
import Control.Monad
import Text.XML
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IORef.Lifted

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Data.Injective
import Network.XMPP.XML
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Presence

data MyPresenceRef m = MyPresenceRef { myPresenceHandler :: Handler m (PresenceEvent XMPPResource)
                                     , myPresence :: IORef (Map XMPPResource Presence)
                                     , myPresenceSession :: StanzaSession m
                                     , myBareAddress :: BareJID
                                     }

myPresencePHandler :: MonadSession m => MyPresenceRef m -> PresenceHandler m
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

myPresenceGet :: MonadSession m => MyPresenceRef m -> m (Map XMPPResource Presence)
myPresenceGet = readIORef . myPresence

myPresenceSetHandler :: MonadSession m => MyPresenceRef m -> (PresenceEvent XMPPResource -> m ()) -> m ()
myPresenceSetHandler (MyPresenceRef {..}) = Handler.set myPresenceHandler

myPresenceSend :: MonadSession m => MyPresenceRef m -> Presence -> m ()
myPresenceSend (MyPresenceRef {..}) (Presence {..}) =
  void $ stanzaSend myPresenceSession OutStanza { ostTo = Nothing
                                                , ostType = OutPresence Nothing
                                                , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceExtended
                                                }

  where priority = element (jcName "priority") [] [NodeContent $ T.pack $ show presencePriority]
        mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
        statuses = maybe [] (localizedElements $ jcName "status") presenceStatus

myPresencePlugin :: MonadSession m => StanzaSession m -> m (PresenceHandler m, MyPresenceRef m)
myPresencePlugin myPresenceSession = do
  myPresence <- newIORef M.empty
  myPresenceHandler <- Handler.new
  let myBareAddress = fullBare $ sessionAddress $ ssSession myPresenceSession
      pref = MyPresenceRef {..}
      phandler = myPresencePHandler pref
  return (phandler, pref)
