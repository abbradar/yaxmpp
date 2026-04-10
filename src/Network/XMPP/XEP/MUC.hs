{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.MUC (
  MUCEvent (..),
  RoomEvent (..),
  MUCHandler,
  MUCRole (..),
  MUCAffiliation (..),
  MUCLeaveReason (..),
  MUCPresenceEvent (..),
  MUCPresence (..),
  MUC (..),
  MUCJoinResult (..),
  MUCAlreadyJoinedError (..),
  MUCHistorySettings (..),
  defaultMUCHistorySettings,
  MUCJoinSettings (..),
  defaultMUCJoinSettings,
  mucJoin,
  MUCAlreadyLeftError (..),
  mucSendPresence,
  mucSlot,
  mucPlugin,
) where

import Control.Monad
import Control.Monad.Catch (Exception, throwM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import GHC.Generics (Generic)
import Text.Read
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)
import UnliftIO.Exception (bracketOnError)
import UnliftIO.IORef
import UnliftIO.MVar

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Injective
import qualified Data.RefMap as RefMap
import qualified Data.Registry.Mutable as RegRef
import Data.Time.XMPP
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML

data MUCEvent
  = MUCJoinedRoom FullJID MUC
  | MUCRejected FullJID StanzaError
  | MUCLeftRoom FullJID MUCLeaveReason
  deriving (Show)

data RoomEvent
  = RoomPresence XMPPResource MUCPresenceEvent
  | RoomSubject
  deriving (Show)

type MUCHandler m = MUC -> RoomEvent -> m ()

data MUC = MUC
  { mucSubject :: Maybe (XMPPResource, Text)
  , mucMembers :: Map XMPPResource Presence
  , mucNick :: XMPPResource
  , mucNonAnonymous :: Bool
  }
  deriving (Show)

data MUCJoinResult
  = MUCJoinFinished MUC
  | MUCJoinError StanzaError
  | MUCJoinStopped [Element]
  deriving (Show)

data MUCRole
  = RoleVisitor
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

data MUCAffiliation
  = AffiliationNone
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

data MUCLeaveReason
  = MUCLeft
  | MUCKicked {kickActor :: XMPPResource, kickReason :: Maybe Text}
  | MUCBanned {banActor :: XMPPResource, banReason :: Maybe Text}
  deriving (Show, Eq)

data MUCPresenceEvent
  = MUCJoined MUCPresence
  | MUCUpdated MUCPresence
  | MUCRemoved MUCLeaveReason
  | MUCRenamed XMPPResource
  deriving (Show)

data MUCPresence = MUCPresence
  { mucPresence :: Presence
  , mucRealJid :: Maybe FullJID
  , mucAffiliation :: MUCAffiliation
  , mucRole :: MUCRole
  }
  deriving (Show)

data PendingMUC m = PendingMUC
  { pmucMembers :: Map XMPPResource MUCPresence
  , pmucNick :: XMPPResource
  , pmucHandler :: MUCHandler m
  , pmucPending :: MVar MUCJoinResult
  }

type MUCValue m = (MUC, MUCHandler m)

type MUCSlot m = Slot m MUCEvent

data MUCState m = MUCState
  { mucRooms :: IORef (Map BareJID (Either (PendingMUC m) (MUCValue m)))
  }

mucNS :: Text
mucName :: Text -> Name
(mucNS, mucName) = namePair "http://jabber.org/protocol/muc"

mucNickNode :: Text
mucNickNode = "x-roomuser-item"

_mucRoomsNS :: Text
_mucRoomsName :: Text -> Name
(_mucRoomsNS, _mucRoomsName) = namePair "http://jabber.org/protocol/muc#rooms"

_mucUserNS :: Text
mucUserName :: Text -> Name
(_mucUserNS, mucUserName) = namePair "http://jabber.org/protocol/muc#user"

mucNickIdentity :: DiscoIdentity
mucNickIdentity =
  DiscoIdentity
    { discoCategory = "conference"
    , discoType = "text"
    }

data MUCAlreadyJoinedError = MUCAlreadyJoinedError
  deriving (Show, Typeable)

instance Exception MUCAlreadyJoinedError

data MUCHistorySettings = MUCHistorySettings
  { histMaxChars :: Maybe Integer
  , histMaxStanzas :: Maybe Integer
  , histSeconds :: Maybe Integer
  , histSince :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

defaultMUCHistorySettings :: MUCHistorySettings
defaultMUCHistorySettings =
  MUCHistorySettings
    { histMaxChars = Nothing
    , histMaxStanzas = Nothing
    , histSeconds = Nothing
    , histSince = Nothing
    }

data MUCJoinSettings = MUCJoinSettings
  { joinHistory :: MUCHistorySettings
  , joinPresence :: Presence
  }
  deriving (Show, Generic)

defaultMUCJoinSettings :: MUCJoinSettings
defaultMUCJoinSettings =
  MUCJoinSettings
    { joinHistory = defaultMUCHistorySettings
    , joinPresence = defaultPresence
    }

mucJoin :: forall m. (MonadStream m) => XMPPPluginsRef m -> FullJID -> MUCJoinSettings -> MUCHandler m -> m (m MUCJoinResult)
mucJoin pluginsRef addr (MUCJoinSettings {joinHistory = MUCHistorySettings {..}, ..}) handler = do
  MUCState {..} <- RegRef.lookupOrFailM (Proxy :: Proxy (MUCState m)) $ pluginsHooksSet pluginsRef
  let session = pluginsSession pluginsRef
  pmucPending <- newEmptyMVar
  let initialRoom =
        PendingMUC
          { pmucNick = fullResource addr
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
      joinRoom =
        getDiscoEntity pluginsRef (fullJidAddress addr) (Just mucNickNode) $ \nickResp -> do
          resource' <- case nickResp of
            Right ent | Just (Just n) <- M.lookup mucNickIdentity $ discoIdentities ent ->
              case resourceFromText $ localizedGet Nothing n of
                Nothing -> fail "mucJoin: invalid resource name proposed by server"
                Just r -> do
                  atomicModifyIORef' mucRooms $ \rooms -> (M.insert roomAddr (Left $ initialRoom {pmucNick = r}) rooms, ())
                  return r
            _ -> return $ fullResource addr
          let historyAttrs =
                catMaybes
                  [ fmap (\i -> ("maxchars", showt i)) histMaxChars
                  , fmap (\i -> ("maxstanzas", showt i)) histMaxStanzas
                  , fmap (\i -> ("seconds", showt i)) histSeconds
                  , fmap (\i -> ("since", utcTimeToXmpp i)) histSince
                  ]
              xElement =
                element
                  (mucName "x")
                  []
                  [ NodeElement $ element (mucName "history") historyAttrs []
                  ]
          presStanza <- presenceStanza pluginsRef $ Just joinPresence {presenceRaw = xElement : presenceRaw joinPresence}
          void $
            stanzaSend session $
              presStanza
                { ostTo = Just $ fullJidAddress $ addr {fullResource = resource'}
                }
  bracketOnError takeRoom (const cleanupRoom) (const joinRoom)
  return $ readMVar pmucPending

data MUCAlreadyLeftError = MUCAlreadyLeftError
  deriving (Show, Typeable)

instance Exception MUCAlreadyLeftError

mucSendPresence :: forall m. (MonadStream m) => XMPPPluginsRef m -> BareJID -> Maybe Presence -> m ()
mucSendPresence pluginsRef addr pres = do
  MUCState {..} <- RegRef.lookupOrFailM (Proxy :: Proxy (MUCState m)) $ pluginsHooksSet pluginsRef
  let session = pluginsSession pluginsRef
  rooms <- readIORef mucRooms
  case M.lookup addr rooms of
    Just (Right (room, _)) -> do
      presStanza <- presenceStanza pluginsRef pres
      _ <-
        stanzaSend session $
          presStanza
            { ostTo = Just $ fullJidAddress $ FullJID addr (mucNick room)
            }
      return ()
    _ -> throwM MUCAlreadyLeftError

type MUCStatusSet = Set Integer

_parseMUCPresence :: ResourceStatus -> Maybe (MUCStatusSet, MUCPresence)
_parseMUCPresence status = do
  let extended = case status of
        ResourceAvailable p -> presenceRaw p
        ResourceUnavailable elems -> elems
  xE <- listToMaybe $ fromChildren extended $/ XC.element (mucUserName "x") &| curElement
  let statusSet = S.fromList $ mapMaybe (readMaybe . T.unpack) $ fromElement xE $/ XC.element (mucUserName "status") &/ attribute "code"
  item <- listToMaybe $ fromElement xE $/ XC.element (mucUserName "item") &| curElement
  mucAffiliation <- getAttr "affiliation" item >>= injFrom
  mucRole <- getAttr "role" item >>= injFrom
  let mucRealJid = getAttr "jid" item >>= (either (const Nothing) Just . xmppAddress) >>= fullJidGet
      mucPresence = case status of
        ResourceAvailable p -> p
        ResourceUnavailable _ -> defaultPresence
  return (statusSet, MUCPresence {..})

data MUCPlugin m = MUCPlugin
  { mucPluginSlot :: MUCSlot m
  , mucPluginState :: MUCState m
  }

instance (MonadStream m) => Handler m InStanza InResponse (MUCPlugin m) where
  tryHandle (MUCPlugin {..}) (InStanza {istFrom = Just (fullJidGet -> Just addr), istType = InPresence (Left err)}) = do
    let MUCState {..} = mucPluginState
        mucEventHandler = mucPluginSlot
    mpromise <- atomicModifyIORef' mucRooms $ \rooms ->
      case M.lookup (fullBare addr) rooms of
        Just (Left pending) | pmucNick pending == fullResource addr -> (M.delete (fullBare addr) rooms, Just $ pmucPending pending)
        _ -> (rooms, Nothing)
    case mpromise of
      Nothing -> return Nothing
      Just promise -> do
        Slot.call mucEventHandler $ MUCRejected addr err
        putMVar promise (MUCJoinError err)
        return $ Just InSilent
  tryHandle (MUCPlugin {..}) (InStanza {istFrom = Just addr@(bareJidGet -> Just bare), istType = InMessage (Right MessageGroupchat), istChildren}) = do
    let MUCState {..} = mucPluginState
    case fromChildren istChildren $/ XC.element (jcName "subject") &| curElement of
      (subjE : _) -> do
        rooms <- readIORef mucRooms
        case M.lookup bare rooms of
          Just (Right (room, handler)) -> do
            let subj = (,mconcat $ fromElement subjE $/ content) <$> addressResource addr
                room' = room {mucSubject = subj}
            atomicWriteIORef mucRooms $ M.insert bare (Right (room', handler)) rooms
            handler room' RoomSubject
            return $ Just InSilent
          _ -> return Nothing
      _ -> return Nothing
  tryHandle _ _ = return Nothing

data MUCHandleResult a
  = NotMUCEvent
  | MUCHandled
  | MUCRun a

instance (MonadStream m) => Handler m PresenceUpdate () (MUCPlugin m) where
  tryHandle _ (AllResourcesOffline _ _) = return Nothing -- MUC rooms don't use bare JID presence
  tryHandle (MUCPlugin {..}) (ResourcePresence addr mpres) = do
    let MUCState {..} = mucPluginState
        mucEventHandler = mucPluginSlot
        bare = fullBare addr
        resource = fullResource addr
    processed <- atomicModifyIORef' mucRooms $ \rooms ->
      case M.lookup bare rooms of
        Just (Right (MUC {..}, _))
          | ResourceUnavailable _ <- mpres
          , resource == mucNick ->
              (M.delete bare rooms, MUCRun $ Slot.call mucEventHandler $ MUCLeftRoom addr MUCLeft)
        Just (Right (room@(MUC {..}), handler)) ->
          case presenceUpdate resource mpres mucMembers of
            Nothing -> (rooms, MUCHandled)
            Just (members, _event) ->
              let room' = room {mucMembers = members}
                  mucPres =
                    MUCPresence
                      { mucPresence = case mpres of ResourceAvailable p -> p; ResourceUnavailable _ -> defaultPresence
                      , mucRealJid = Nothing
                      , mucAffiliation = AffiliationNone
                      , mucRole = RoleNone
                      }
               in (M.insert bare (Right (room', handler)) rooms, MUCRun $ handler room' $ RoomPresence resource (MUCUpdated mucPres))
        Just (Left pending) ->
          case mpres of
            ResourceAvailable pres ->
              let mucPres = MUCPresence {mucPresence = pres, mucRealJid = Nothing, mucAffiliation = AffiliationNone, mucRole = RoleNone}
                  members = M.insert resource mucPres $ pmucMembers pending
                  pending' = pending {pmucMembers = members}
                  room' =
                    MUC
                      { mucMembers = M.map mucPresence members
                      , mucSubject = Nothing
                      , mucNick = pmucNick pending
                      , mucNonAnonymous = False
                      }
                  joinRoom = do
                    Slot.call mucEventHandler $ MUCJoinedRoom addr room'
                    putMVar (pmucPending pending) (MUCJoinFinished room')
               in if resource == pmucNick pending
                    then (M.insert bare (Right (room', pmucHandler pending)) rooms, MUCRun joinRoom)
                    else (M.insert bare (Left pending') rooms, MUCHandled)
            ResourceUnavailable err
              | resource == pmucNick pending ->
                  let leaveRoom = do
                        Slot.call mucEventHandler $ MUCLeftRoom addr MUCLeft
                        putMVar (pmucPending pending) (MUCJoinStopped err)
                   in (M.delete bare rooms, MUCRun leaveRoom)
            _ -> (rooms, MUCHandled)
        Nothing -> (rooms, NotMUCEvent)
    case processed of
      MUCRun action -> action >> return (Just ())
      MUCHandled -> return (Just ())
      NotMUCEvent -> return Nothing

mucSlot :: (MonadStream m) => XMPPPluginsRef m -> m (MUCSlot m)
mucSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

mucPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
mucPlugin pluginsRef = do
  mucRooms <- newIORef M.empty
  let mucPluginState = MUCState {..}
  mucPluginSlot <- Slot.new
  let plugin :: MUCPlugin m = MUCPlugin {..}
  RegRef.insertNewOrFailM mucPluginState $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM mucPluginSlot $ pluginsHooksSet pluginsRef
  let discoInfo = emptyDiscoInfo {discoIEntity = emptyDiscoEntity {discoFeatures = S.singleton mucNS}}
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  pHandlers <- presenceHandlers pluginsRef
  HL.pushNewOrFailM plugin pHandlers
  dInfos <- discoInfos pluginsRef
  void $ RefMap.add dInfos $ return discoInfo
