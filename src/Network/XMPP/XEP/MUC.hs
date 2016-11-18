module Network.XMPP.XEP.MUC
  ( MUCEvent(..)
  , RoomEvent(..)
  , MUCHandler
  , MUC(..)
  , MUCRef
  , MUCAlreadyJoinedError(..)
  , MUCHistorySettings(..)
  , MUCJoinSettings(..)
  , mucJoin
  , MUCAlreadyLeftError(..)
  , mucSendPresence
  , mucSetHandler
  , mucPlugin
  ) where

import Data.Maybe
import Control.Monad
import Data.Typeable
import Control.Exception.Lifted (Exception, throw)
import GHC.Generics (Generic)
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Concurrent.MVar.Lifted
import Data.IORef.Lifted
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.Default.Class

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.XML
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.XEP.DateTime
import Network.XMPP.XEP.Disco

data MUCEvent = MUCJoined FullJID MUC
              | MUCRejected FullJID StanzaError
              | MUCLeft FullJID [Element]
              deriving (Show, Eq)

data RoomEvent = RoomPresence (PresenceEvent XMPPResource)
               | RoomSubject
               deriving (Show, Eq)

type MUCHandler m = MUC -> RoomEvent -> m ()

data MUC = MUC { mucSubject :: Text
               , mucMembers :: Map XMPPResource Presence
               , mucNick :: XMPPResource
               }
         deriving (Show, Eq)

data MUCRef m = MUCRef { mucRooms :: IORef (Map BareJID (MUC, MUCHandler m))
                       , mucPending :: MVar (Map BareJID (MUC, MUCHandler m))
                       , mucEventHandler :: Handler m MUCEvent
                       , mucSession :: StanzaSession m
                       }

mucNS :: Text
mucName :: Text -> Name
(mucNS, mucName) = namePair "http://jabber.org/protocol/muc"

mucNickNode :: Text
mucNickNode = "x-roomuser-item"

mucRoomsNS :: Text
mucRoomsName :: Text -> Name
(mucRoomsNS, mucRoomsName) = namePair "http://jabber.org/protocol/muc#rooms"

mucUserNS :: Text
mucUserName :: Text -> Name
(mucUserNS, mucUserName) = namePair "http://jabber.org/protocol/muc#user"

mucNickIdentity :: DiscoIdentity
mucNickIdentity = DiscoIdentity { discoCategory = "conference"
                                , discoType = "text"
                                }

data MUCAlreadyJoinedError = MUCAlreadyJoinedError
                           deriving (Show, Typeable)

instance Exception MUCAlreadyJoinedError where

data MUCHistorySettings = MUCHistorySettings { histMaxChars :: Maybe Integer
                                             , histMaxStanzas :: Maybe Integer
                                             , histSeconds :: Maybe Integer
                                             , histSince :: Maybe UTCTime
                                             }
                        deriving (Show, Eq, Generic)

instance Default MUCHistorySettings where

data MUCJoinSettings = MUCJoinSettings { joinHistory :: MUCHistorySettings
                                       , joinPresence :: Presence
                                       }
                     deriving (Show, Eq, Generic)

instance Default MUCJoinSettings where

mucJoin :: MonadSession m => MUCRef m -> FullJID -> MUCJoinSettings -> MUCHandler m -> m ()
mucJoin (MUCRef {..}) addr (MUCJoinSettings { joinHistory = MUCHistorySettings {..}, .. }) handler = do
  nickResp <- getDiscoEntity mucSession (fullJidAddress addr) (Just mucNickNode)
  resource' <- case nickResp of
    Right ent | Just (Just n) <- M.lookup mucNickIdentity $ discoIdentities ent ->
                case resourceFromText $ localizedGet Nothing n of
                  Nothing -> fail "mucJoin: invalid resource name proposed by server"
                  Just r -> return r
    _ -> return $ fullResource addr
  modifyMVar_ mucPending $ \pending -> do
    when (M.member (fullBare addr) pending) $ throw MUCAlreadyJoinedError
    rooms <- readIORef mucRooms
    when (M.member (fullBare addr) rooms) $ throw MUCAlreadyJoinedError
    let historyAttrs = catMaybes [ fmap (\i -> ("maxchars", T.pack $ show i)) histMaxChars
                                 , fmap (\i -> ("maxstanzas", T.pack $ show i)) histMaxStanzas
                                 , fmap (\i -> ("seconds", T.pack $ show i)) histSeconds
                                 , fmap (\i -> ("since", utcTimeToXmpp i)) histSince
                                 ]
        xElement = element (mucName "x") []
                   [ NodeElement $ element (mucName "history") historyAttrs []
                   ]
        presStanza = presenceStanza $ Just joinPresence { presenceExtended = xElement : presenceExtended joinPresence }
    _ <- stanzaSend mucSession $ presStanza { ostTo = Just $ fullJidAddress $ addr { fullResource = resource' }
                                           }
    let pmuc = MUC { mucSubject = ""
                   , mucMembers = M.empty
                   , mucNick = resource'
                   , ..
                   }
    return $ M.insert (fullBare addr) (pmuc, handler) pending

data MUCAlreadyLeftError = MUCAlreadyLeftError
                         deriving (Show, Typeable)

instance Exception MUCAlreadyLeftError where

mucSendPresence :: MonadSession m => MUCRef m -> BareJID -> Maybe Presence -> m ()
mucSendPresence (MUCRef {..}) addr pres = do
  rooms <- readIORef mucRooms
  case M.lookup addr rooms of
    Just (room, _) -> do
      let presStanza = presenceStanza pres
      _ <- stanzaSend mucSession $ presStanza { ostTo = Just $ fullJidAddress $ FullJID addr (mucNick room)
                                             }
      return ()
    Nothing -> throw MUCAlreadyLeftError

mucInHandler :: MonadSession m => MUCRef m -> PluginInHandler m
mucInHandler (MUCRef {..}) (InStanza { istFrom = Just (fullJidGet -> Just addr), istType = InPresence (Left err) }) = do
  found <- modifyMVar mucPending $ \pending ->
    if fullBare addr `M.member` pending
    then return (M.delete (fullBare addr) pending, True)
    else return (pending, False)
  if found
    then do
      Handler.call mucEventHandler $ MUCRejected addr err
      return $ Just Nothing
    else return Nothing
mucInHandler (MUCRef {..}) (InStanza { istFrom = Just (fullJidGet -> Just addr), istType = InMessage (Right MessageGroupchat), istChildren }) =
  case fromChildren istChildren $/ XC.element (jcName "subject") &| curElement of
    [subjE] -> do
      let bare = fullBare addr
          resource = fullResource addr
          subj = mconcat $ fromElement subjE $/ content
      rooms <- readIORef mucRooms
      case M.lookup bare rooms of
        Just (room, handler) | mucNick room == resource -> do
                                 let room' = room { mucSubject = subj }
                                 writeIORef mucRooms $ M.insert bare (room', handler) rooms
                                 handler room' RoomSubject
                                 return $ Just Nothing
        _ -> return Nothing
    _ -> return Nothing
mucInHandler _ _ = return Nothing

mucPresenceHandler :: MonadSession m => MUCRef m -> PresenceHandler m
mucPresenceHandler (MUCRef {..}) addr mpres = do
  let bare = fullBare addr
      resource = fullResource addr
  mProcessed <- modifyMVar mucPending $ \pending ->
    case M.lookup bare pending of
      Just (room@(MUC {..}), handler) -> case mpres of
        Right pres ->
          let members = M.insert resource pres mucMembers
              room' = room { mucMembers = members }
          in if resource == mucNick
             then do
               modifyIORef mucRooms $ M.insert bare (room', handler)
               return (M.delete bare pending, Just $ Just $ MUCJoined addr room')
             else return (M.insert bare (room', handler) pending, Just Nothing)
        Left err | resource == mucNick -> return (M.delete bare pending, Just $ Just $ MUCLeft addr err)
        Left _ ->
          let members = M.delete resource mucMembers
              room' = room { mucMembers = members }
          in return (M.insert bare (room', handler) pending, Just Nothing)
      Nothing -> return (pending, Nothing)
  case mProcessed of
    Just (Just msg) -> do
      Handler.call mucEventHandler msg
      return True
    Just Nothing -> return True
    Nothing -> do
      rooms <- readIORef mucRooms
      case M.lookup bare rooms of
        Just (MUC {..}, _) | Left err <- mpres, resource == mucNick -> do
                               writeIORef mucRooms $ M.delete bare rooms
                               Handler.call mucEventHandler $ MUCLeft addr err
                               return True
        Just (room@(MUC {..}), handler) -> do
          case presenceUpdate resource mpres mucMembers of
            Nothing -> return False
            Just (members, event) -> do
              let room' = room { mucMembers = members }
              writeIORef mucRooms $ M.insert bare (room', handler) rooms
              handler room' $ RoomPresence event
              return True
        Nothing -> return False

mucSetHandler :: MonadSession m => MUCRef m -> (MUCEvent -> m ()) -> m ()
mucSetHandler (MUCRef {..}) = Handler.set mucEventHandler

mucPlugin :: MonadSession m => StanzaSession m -> m (XMPPPlugin m, PresenceHandler m, DiscoPlugin, MUCRef m)
mucPlugin mucSession = do
  mucRooms <- newIORef M.empty
  mucPending <- newMVar M.empty
  mucEventHandler <- Handler.new
  let mref = MUCRef {..}
      xmppPlugin = def { pluginInHandler = mucInHandler mref
                       }
      discoHandler = def { discoPEntity = def { discoFeatures = S.singleton mucNS }
                         , discoPChildren = M.singleton mucRoomsNS (def, M.empty)
                         }
      presHandler = mucPresenceHandler mref
  return (xmppPlugin, presHandler, discoHandler, mref)
