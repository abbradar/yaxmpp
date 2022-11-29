{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.MUC
  ( MUCEvent(..)
  , RoomEvent(..)
  , MUCHandler
  , MUCRole(..)
  , MUCAffiliation(..)
  , MUCLeaveReason(..)
  , MUCPresenceEvent(..)
  , MUCPresence(..)
  , MUC(..)
  , MUCJoinResult(..)
  , MUCRef
  , MUCAlreadyJoinedError(..)
  , MUCHistorySettings(..)
  , defaultMUCHistorySettings
  , MUCJoinSettings(..)
  , defaultMUCJoinSettings
  , mucJoin
  , MUCAlreadyLeftError(..)
  , mucSendPresence
  , mucAddHandler
  , mucDeleteHandler
  , mucPlugin
  ) where

import Data.Maybe
import Control.Monad
import Data.Typeable
import Text.Read
import GHC.Generics (Generic)
import Data.Time.Clock
import Data.Text (Text)
import Control.Monad.Catch (Exception, throwM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import UnliftIO.IORef
import UnliftIO.MVar
import UnliftIO.Exception (bracketOnError)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)

import Data.Injective
import Data.Time.XMPP
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Network.XMPP.XML
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.XEP.Disco

data MUCEvent = MUCJoinedRoom FullJID MUC
              | MUCRejected FullJID StanzaError
              | MUCLeftRoom FullJID MUCLeaveReason
              deriving (Show, Eq)

data RoomEvent = RoomPresence XMPPResource MUCPresenceEvent
               | RoomSubject
               deriving (Show, Eq)

type MUCHandler m = MUC -> RoomEvent -> m ()

data MUC = MUC { mucSubject :: Maybe (XMPPResource, Text)
               , mucMembers :: Map XMPPResource Presence
               , mucNick :: XMPPResource
               , mucNonAnonymous :: Bool
               }
         deriving (Show, Eq)

data MUCJoinResult = MUCJoinFinished MUC
                   | MUCJoinError StanzaError
                   | MUCJoinStopped [Element]
                   deriving (Show, Eq)

data MUCRole = RoleVisitor
             | RoleParticipant
             | RoleModerator
             | RoleNone
             deriving (Show, Eq, Bounded, Enum)

instance Injective MUCRole Text where
  injTo x = case x of
    RoleVisitor -> "visitor"
    RoleParticipant -> "participant"
    RoleModerator -> "moderator"
    RoleNone -> "none"

data MUCAffiliation = AffiliationNone
                    | AffiliationMember
                    | AffiliationAdmin
                    | AffiliationOwner
                    | AffiliationOutcast
                    deriving (Show, Eq, Bounded, Enum)

instance Injective MUCAffiliation Text where
  injTo x = case x of
    AffiliationNone -> "none"
    AffiliationMember -> "member"
    AffiliationAdmin -> "admin"
    AffiliationOwner -> "owner"
    AffiliationOutcast -> "outcast"

data MUCLeaveReason = MUCLeft
                    | MUCKicked { kickActor :: XMPPResource, kickReason :: Maybe Text }
                    | MUCBanned { banActor :: XMPPResource, banReason :: Maybe Text }
                    deriving (Show, Eq)



data MUCPresenceEvent = MUCJoined MUCPresence
                      | MUCUpdated MUCPresence
                      | MUCRemoved MUCLeaveReason
                      | MUCRenamed XMPPResource
                      deriving (Show, Eq)

data MUCPresence = MUCPresence { mucPresence :: Presence
                               , mucRealJid :: Maybe FullJID
                               , mucAffiliation :: MUCAffiliation
                               , mucRole :: MUCRole
                               }
                 deriving (Show, Eq)

data PendingMUC m = PendingMUC { pmucMembers :: Map XMPPResource MUCPresence
                               , pmucNick :: XMPPResource
                               , pmucHandler :: MUCHandler m
                               , pmucPending :: MVar MUCJoinResult
                               }

type MUCValue m = (MUC, MUCHandler m)

data MUCRef m = MUCRef { mucRooms :: IORef (Map BareJID (Either (PendingMUC m) (MUCValue m)))
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

defaultMUCHistorySettings :: MUCHistorySettings
defaultMUCHistorySettings = MUCHistorySettings { histMaxChars = Nothing
                                               , histMaxStanzas = Nothing
                                               , histSeconds = Nothing
                                               , histSince = Nothing
                                               }

data MUCJoinSettings = MUCJoinSettings { joinHistory :: MUCHistorySettings
                                       , joinPresence :: Presence
                                       }
                     deriving (Show, Eq, Generic)

defaultMUCJoinSettings :: MUCJoinSettings
defaultMUCJoinSettings = MUCJoinSettings { joinHistory = defaultMUCHistorySettings
                                         , joinPresence = defaultPresence
                                         }

mucJoin :: MonadStream m => MUCRef m -> FullJID -> MUCJoinSettings -> MUCHandler m -> m (m MUCJoinResult)
mucJoin (MUCRef {..}) addr (MUCJoinSettings { joinHistory = MUCHistorySettings {..}, .. }) handler = do
  pmucPending <- newEmptyMVar
  let initialRoom = PendingMUC { pmucNick = fullResource addr
                               , pmucMembers = M.empty
                               , pmucHandler = handler
                               , ..
                               }
      roomAddr = fullBare addr
      takeRoom = do
        good <- atomicModifyIORef' mucRooms $ \rooms ->
          case M.lookup roomAddr rooms of
            Nothing -> (M.insert roomAddr (Left initialRoom) rooms, True)
            Just _ -> (rooms, False)
        unless good $ throwM MUCAlreadyJoinedError
      cleanupRoom = atomicModifyIORef' mucRooms $ \rooms -> (M.delete roomAddr rooms, ())
      joinRoom = do
        nickResp <- getDiscoEntity mucSession (fullJidAddress addr) (Just mucNickNode)
        resource' <- case nickResp of
          Right ent | Just (Just n) <- M.lookup mucNickIdentity $ discoIdentities ent ->
                      case resourceFromText $ localizedGet Nothing n of
                        Nothing -> fail "mucJoin: invalid resource name proposed by server"
                        Just r -> do
                          atomicModifyIORef' mucRooms $ \rooms -> (M.insert roomAddr (Left $ initialRoom { pmucNick = r }) rooms, ())
                          return r
          _ -> return $ fullResource addr
        let historyAttrs = catMaybes [ fmap (\i -> ("maxchars", showt i)) histMaxChars
                                     , fmap (\i -> ("maxstanzas", showt i)) histMaxStanzas
                                     , fmap (\i -> ("seconds", showt i)) histSeconds
                                     , fmap (\i -> ("since", utcTimeToXmpp i)) histSince
                                     ]
            xElement = element (mucName "x") []
                       [ NodeElement $ element (mucName "history") historyAttrs []
                       ]
            presStanza = presenceStanza $ Just joinPresence { presenceExtended = xElement : presenceExtended joinPresence }
        void $ stanzaSend mucSession $ presStanza { ostTo = Just $ fullJidAddress $ addr { fullResource = resource' }
                                                  }
  bracketOnError takeRoom cleanupRoom joinRoom
  return $ readMVar pmucPending

data MUCAlreadyLeftError = MUCAlreadyLeftError
                         deriving (Show, Typeable)

instance Exception MUCAlreadyLeftError where

mucSendPresence :: MonadStream m => MUCRef m -> BareJID -> Maybe Presence -> m ()
mucSendPresence (MUCRef {..}) addr pres = do
  rooms <- readIORef mucRooms
  case M.lookup addr rooms of
    Just (Right (room, _)) -> do
      let presStanza = presenceStanza pres
      _ <- stanzaSend mucSession $ presStanza { ostTo = Just $ fullJidAddress $ FullJID addr (mucNick room)
                                              }
      return ()
    _ -> throwM MUCAlreadyLeftError

type MUCStatusSet = Set Integer

parseMUCPresence :: Either [Element] Presence -> Maybe (MUCStatusSet, MUCPresenceEvent, MUCPresence)
parseMUCPresence pres = do
  let extended =
        case pres of
          Left extended -> extended
          Right pres -> presenceExtended pres
  xE <- listToMaybe $ fromChildren extended $/ XC.element (mucUserName "x") &| curElement
  let statusSet = S.fromList $ mapMaybe (readMaybe . T.unpack) $ fromElement xE $/ XC.element (mucUserName "status") &/ attribute "code"
  item <- listToMaybe $ fromElement xE $/ XC.element (mucUserName "item") &| curElement
  mucAffiliation <- getAttr "affiliation" item >>= injTo
  mucRole <- getAttr "role" item >>= injTo
  mucRealJid <- getAttr "jid" item
  if 307 `S.member`
  return (statusSet, )                       --, rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content

mucInHandler :: MonadStream m => MUCRef m -> PluginInHandler m
mucInHandler (MUCRef {..}) (InStanza { istFrom = Just (fullJidGet -> Just addr), istType = InPresence (Left err) }) = do
  mpromise <- atomicModifyIORef' mucRooms $ \rooms ->
    case M.lookup (fullBare addr) rooms of
      Just (Left pending) | pmucNick pending == fullResource addr -> (M.delete (fullBare addr) rooms, Just $ pmucPending pending)
      _ -> (rooms, Nothing)
  case mpromise of
    Nothing -> return Nothing
    Just promise -> do
      Handler.call mucEventHandler $ MUCRejected addr err
      putMVar promise (MUCJoinError err)
      return $ Just InSilent
mucInHandler (MUCRef {..}) (InStanza { istFrom = Just addr@(bareJidGet -> Just bare), istType = InMessage (Right MessageGroupchat), istChildren }) =
  case fromChildren istChildren $/ XC.element (jcName "subject") &| curElement of
    (subjE:_) -> do
      rooms <- readIORef mucRooms
      case M.lookup bare rooms of
        Just (Right (room, handler)) -> do
          let subj = (, mconcat $ fromElement subjE $/ content) <$> addressResource addr
              room' = room { mucSubject = subj }
          atomicWriteIORef mucRooms $ M.insert bare (Right (room', handler)) rooms
          handler room' RoomSubject
          return $ Just InSilent
        _ -> return Nothing
    _ -> return Nothing
mucInHandler _ _ = return Nothing

data MUCHandleResult a = NotMUCEvent
                       | MUCHandled
                       | MUCRun a



mucPresenceHandler :: MonadStream m => MUCRef m -> PresenceHandler m
mucPresenceHandler (MUCRef {..}) addr mpres = do
  let bare = fullBare addr
      resource = fullResource addr
  processed <- atomicModifyIORef' mucRooms $ \rooms ->
        case M.lookup bare rooms of
          Just (Right (MUC {..}, _)) | Left err <- mpres, resource == mucNick -> (M.delete bare rooms, MUCRun $ Handler.call mucEventHandler $ MUCLeft addr err)
          Just (Right (room@(MUC {..}), handler)) -> do
            case presenceUpdate resource mpres mucMembers of
              Nothing -> (rooms, MUCHandled)
              Just (members, event) ->
                let room' = room { mucMembers = members }
                in (M.insert bare (Right (room', handler)) rooms, MUCRun $ handler room' $ RoomPresence event)
          Just (Left pending) ->
            case mpres of
              Right pres ->
                let mucMembers = M.insert resource pres $ pmucMembers pending
                    pending' = pending { pmucMembers = mucMembers }
                    room' = MUC { mucMembers = mucMembers
                                , mucSubject = Nothing
                                , mucNick = pmucNick pending
                                , mucNonAnonymous = False
                                }
                    joinRoom = do
                      Handler.call mucEventHandler $ MUCJoined addr room'
                      putMVar (pmucPending pending) (MUCJoinFinished room')
                in if resource == pmucNick pending
                then (M.insert bare (Right (room', pmucHandler pending)) rooms, MUCRun joinRoom)
                else (M.insert bare (Left pending') rooms, MUCHandled)
              Left err | resource == pmucNick pending ->
                         let leaveRoom = do
                               Handler.call mucEventHandler $ MUCLeft addr err
                               putMVar (pmucPending pending) (MUCJoinStopped err)
                         in (M.delete bare rooms, MUCRun leaveRoom)
              Left _ ->
                let members = M.delete resource mucMembers
                    room' = room { mucMembers = members }
                in return (M.insert bare (room', handler) pending, Just Nothing)
          Nothing -> return NotMUCEvent
  if rProcessed then return True else do
    mProcessed <- modifyMVar mucPending $ \pending ->
      case M.lookup bare pending of

        Nothing -> return (pending, Nothing)
    case mProcessed of
      Just (Just msg) -> do
        Handler.call mucEventHandler msg
        return True
      Just Nothing -> return True
      Nothing -> return False

mucDeleteHandler :: MonadStream m => MUCRef m -> (MUCEvent -> m ()) -> m ()
mucDeleteHandler (MUCRef {..}) = Handler.set mucEventHandler

mucDeleteHandler :: MonadStream m => MUCRef m -> (MUCEvent -> m ()) -> m ()
mucDeleteHandler (MUCRef {..}) = Handler.set mucEventHandler

mucPlugin :: MonadStream m => StanzaSession m -> m (XMPPPlugin m, PresenceHandler m, DiscoPlugin, MUCRef m)
mucPlugin mucSession = do
  mucRooms <- newIORef M.empty
  mucPending <- newMVar M.empty
  mucEventHandler <- Handler.new
  let mref = MUCRef {..}
      xmppPlugin = emptyPlugin { pluginInHandler = mucInHandler mref
                               }
      discoHandler = emptyDiscoPlugin { discoPEntity = emptyDiscoEntity { discoFeatures = S.singleton mucNS }
                                      , discoPChildren = M.singleton mucRoomsNS (emptyDiscoEntity, M.empty)
                                      }
      presHandler = mucPresenceHandler mref
  return (xmppPlugin, presHandler, discoHandler, mref)
