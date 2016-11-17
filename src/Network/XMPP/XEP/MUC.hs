module Network.XMPP.XEP.MUC
  ( MUCEvent(..)
  , MUCHandler
  , MUC(..)
  , MUCRef
  , mucJoin
  , mucSetHandler
  , mucPlugin
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Concurrent.MVar.Lifted
import Data.IORef.Lifted
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.XML
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Presence
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

mucJoin :: MonadSession m => MUCRef m -> FullJID -> MUCHandler m -> m ()
mucJoin (MUCRef {..}) addr handler = do
  rooms <- readIORef mucRooms
  when (M.member (fullBare addr) rooms) $ fail "mucJoin: already present in the room"
  nickResp <- getDiscoEntity mucSession (fullJidAddress addr) (Just mucNickNode)
  resource' <- case nickResp of
    Right ent | Just (Just n) <- M.lookup mucNickIdentity $ discoIdentities ent ->
                case resourceFromText $ localizedGet Nothing n of
                  Nothing -> fail "mucJoin: invalid resource name proposed by server"
                  Just r -> return r
    _ -> return $ fullResource addr
  modifyMVar_ mucPending $ \pending -> do
    when (M.member (fullBare addr) pending) $ fail "mucJoin: already pending to the room"
    _ <- stanzaSend mucSession OutStanza { ostTo = Just $ fullJidAddress $ addr { fullResource = resource' }
                                        , ostType = OutPresence Nothing
                                        , ostChildren = [closedElement $ mucName "x"]
                                        }
    let pmuc = MUC { mucSubject = ""
                   , mucMembers = M.empty
                   , mucNick = resource'
                   , ..
                   }
    return $ M.insert (fullBare addr) (pmuc, handler) pending

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
             then return (M.delete bare pending, Just (Just (room', handler), Just $ MUCJoined addr room'))
             else return (M.insert bare (room', handler) pending, Nothing)
        Left err | resource == mucNick -> return (M.delete bare pending, Just (Nothing, Just $ MUCLeft addr err))
        Left _ ->
          let members = M.delete resource mucMembers
              room' = room { mucMembers = members }
          in return (M.insert bare (room', handler) pending, Just (Nothing, Nothing))
      Nothing -> return (pending, Nothing)
  case mProcessed of
    Just (mRoomItem, mMsg) -> do
      case mRoomItem of
        Just roomItem -> modifyIORef mucRooms $ M.insert bare roomItem
        Nothing -> return ()
      case mMsg of
        Just msg -> Handler.call mucEventHandler msg
        Nothing -> return ()
      return True
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
