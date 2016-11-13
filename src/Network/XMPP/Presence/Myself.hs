module Network.XMPP.Presence.Myself
  ( MyPresenceRef
  , myPresenceGet
  , myPresenceSend
  , myPresenceSubscribe
  , myPresencePlugin
  ) where

import Data.Maybe
import Control.Monad
import Text.XML
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IORef.Lifted

import Data.Injective
import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Network.XMPP.XML
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Presence

data MyPresenceRef m = MyPresenceRef { myPresenceSignal :: Signal m (XMPPResource, Maybe Presence)
                                     , myPresence :: IORef (Map XMPPResource Presence)
                                     , myPresenceSession :: StanzaSession m
                                     , myBareAddress :: BareJID
                                     }

myPresenceHandler :: MonadSession m => MyPresenceRef m -> PresenceHandler m
myPresenceHandler (MyPresenceRef {..}) (FullJID {..}) pres
  | myBareAddress == fullBare = do
      modifyIORef myPresence $ M.update (const pres) fullResource
      Signal.emit myPresenceSignal (fullResource, pres)
      return True
myPresenceHandler _ _ _ = return False

myPresenceGet :: MonadSession m => MyPresenceRef m -> m (Map XMPPResource Presence)
myPresenceGet = readIORef . myPresence

myPresenceSend :: MonadSession m => MyPresenceRef m -> Presence -> m ()
myPresenceSend (MyPresenceRef {..}) (Presence {..}) =
  void $ stanzaSend myPresenceSession OutStanza { ostTo = Nothing
                                                , ostType = OutPresence Nothing
                                                , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceExtended
                                                }

  where priority = element (jcName "priority") [] [NodeContent $ T.pack $ show presencePriority]
        mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
        statuses = maybe [] (localizedElements $ jcName "status") presenceStatus

myPresenceSubscribe :: MonadSession m => MyPresenceRef m -> ((XMPPResource, Maybe Presence) -> m ()) -> m ()
myPresenceSubscribe (MyPresenceRef {..}) = Signal.subscribe myPresenceSignal

myPresencePlugin :: MonadSession m => StanzaSession m -> m (PresenceHandler m, MyPresenceRef m)
myPresencePlugin myPresenceSession = do
  myPresence <- newIORef M.empty
  myPresenceSignal <- Signal.empty
  let myBareAddress = fullBare $ sessionAddress $ ssSession myPresenceSession
      pref = MyPresenceRef {..}
      phandler = myPresenceHandler pref
  return (phandler, pref)
