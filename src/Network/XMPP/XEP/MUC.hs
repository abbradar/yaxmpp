{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.MUC (
  MUCEvent (..),
  RoomEvent (..),
  MUCRoomSlot,
  MUCRef,
  mucRoom,
  mucSlot,
  mucState,
  MUCRole (..),
  MUCAffiliation (..),
  MUCLeaveReason (..),
  MUCPresenceEvent (..),
  MUCStatus (..),
  MUCStatusSet,
  MUCInfo (..),
  MUCPresenceFilter (..),
  MUC (..),
  MUCJoinResult (..),
  MUCAlreadyJoinedError (..),
  MUCHistorySettings (..),
  defaultMUCHistorySettings,
  MUCJoinSettings (..),
  defaultMUCJoinSettings,
  mucGetRegisteredNick,
  mucJoin,
  MUCAlreadyLeftError (..),
  mucSendPresence,
  MUCSlot,
  MUCPlugin,
  mucPluginSlot,
  getMUCPlugin,
  lookupMUCRoom,
  mucPlugin,
) where

import Control.Monad
import Control.Monad.Catch (Exception, throwM)
import Control.Monad.Logger
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate (i)
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

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.ClassBox (Unconstrained)
import Data.Injective
import Data.List (partition)
import qualified Data.Registry as Reg
import Data.Registry.Mutable (RegistryRef)
import qualified Data.Registry.Mutable as RegRef
import Data.Time.XMPP
import Network.XMPP.Address
import Network.XMPP.Filter (FilterReceive (..))
import qualified Network.XMPP.Filter as Filter
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML

data MUCEvent m
  = MUCJoinedRoom FullJID (MUCRef m)
  | MUCRejected FullJID StanzaError
  | MUCLeftRoom FullJID MUCLeaveReason
  | {- | A per-room event mirrored onto 'mucPluginSlot' so subscribers can
    listen across all joined rooms without subscribing to each room's
    'mucSlot' individually.
    -}
    MUCRoomEvent BareJID (MUCRef m) (RoomEvent m)
  deriving (Show)

data RoomEvent m
  = RoomPresence XMPPResource (MUCPresenceEvent m)
  | RoomSubject
  | {- | Configuration change broadcast from the room (XEP-0045 §10.9): a
    groupchat message carrying only @\<x xmlns="muc#user"\>@ with status
    codes describing the change.
    -}
    RoomConfigChanged MUCStatusSet
  deriving (Show)

-- | Per-room slot fired with the latest 'MUCRef' and the event that produced it.
type MUCRoomSlot m = Slot m (MUCRef m, RoomEvent m)

data MUC m = MUC
  { mucSubject :: Maybe (XMPPResource, Text)
  , mucMembers :: Map XMPPResource (PresenceRef m)
  , mucNick :: XMPPResource
  , mucNonAnonymous :: Bool
  }
  deriving (Show)

data MUCJoinResult m
  = MUCJoinFinished (MUCRef m)
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

data MUCPresenceEvent m
  = MUCJoined (PresenceRef m)
  | MUCUpdated (PresenceRef m)
  | MUCRemoved MUCLeaveReason
  | MUCRenamed XMPPResource
  deriving (Show)

{- | (Incomplete) enum of XEP-0045 §10 status codes that the library
recognizes.Unknown codes parsed from a presence remain as their raw
integer in 'MUCStatusSet'.
-}
data MUCStatus
  = -- | 102: room now shows unavailable members.
    MUCStatusShowingUnavailable
  | -- | 103: room no longer shows unavailable members.
    MUCStatusHidingUnavailable
  | -- | 104: a non-privacy-related configuration change occurred.
    MUCStatusNonPrivacyConfigChanged
  | -- | 170: room logging is now enabled.
    MUCStatusLoggingEnabled
  | -- | 171: room logging is now disabled.
    MUCStatusLoggingDisabled
  | -- | 172: room is now non-anonymous.
    MUCStatusNowNonAnonymous
  | -- | 173: room is now semi-anonymous.
    MUCStatusNowSemiAnonymous
  | -- | 174: room is now fully-anonymous.
    MUCStatusNowFullyAnonymous
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Injective MUCStatus Int where
  injTo s = case s of
    MUCStatusShowingUnavailable -> 102
    MUCStatusHidingUnavailable -> 103
    MUCStatusNonPrivacyConfigChanged -> 104
    MUCStatusLoggingEnabled -> 170
    MUCStatusLoggingDisabled -> 171
    MUCStatusNowNonAnonymous -> 172
    MUCStatusNowSemiAnonymous -> 173
    MUCStatusNowFullyAnonymous -> 174

type MUCStatusSet = Set (Either Int MUCStatus)

-- | MUC-specific presence information stored in 'presenceExtended' by 'MUCPresenceFilter'.
data MUCInfo = MUCInfo
  { mucInfoStatus :: MUCStatusSet
  , mucInfoRealJid :: Maybe FullJID
  , mucInfoAffiliation :: MUCAffiliation
  , mucInfoRole :: MUCRole
  }
  deriving (Show, Typeable)

data MUCPresenceFilter = MUCPresenceFilter

{- | Receive-only filter: parses MUC @\<x xmlns=\"muc#user\"/\>@ on incoming
presences. The send side is a no-op because the server is the sole
authority for these elements (XEP-0045 §7.2).
-}
instance (MonadStream m) => FilterReceive m FullJID Presence StanzaError MUCPresenceFilter where
  filterReceive _ _ pres = case extractMUCInfo (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0045 MUC presence: #{err}|]
      return $ Right pres
    Right (mInfo, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\info -> Reg.insert info ext) mInfo
       in return $ Right $ pres {presenceRaw = raw', presenceExtended = ext'}

extractMUCInfo :: [Element] -> Either String (Maybe MUCInfo, [Element])
extractMUCInfo elems =
  let (xElems, rest) = partition (\e -> elementName e == mucUserName "x") elems
   in case xElems of
        [] -> Right (Nothing, rest)
        (xE : _) -> (\info -> (Just info, rest)) <$> parseMUCInfo xE

parseStatusCodes :: Element -> MUCStatusSet
parseStatusCodes xE =
  let parseStatus t = do
        n <- readMaybe (T.unpack t)
        return $ maybe (Left n) Right (injFrom n)
   in S.fromList $ mapMaybe parseStatus $ fromElement xE $/ XC.element (mucUserName "status") &/ attribute "code"

parseMUCInfo :: Element -> Either String MUCInfo
parseMUCInfo xE = do
  let mucInfoStatus = parseStatusCodes xE
  item <- maybe (Left "missing <item> in <x>") Right $ listToMaybe $ fromElement xE $/ XC.element (mucUserName "item") &| curElement
  affText <- maybe (Left "missing affiliation attribute") Right $ getAttr "affiliation" item
  mucInfoAffiliation <- maybe (Left $ "invalid affiliation: " <> T.unpack affText) Right $ injFrom affText
  roleText <- maybe (Left "missing role attribute") Right $ getAttr "role" item
  mucInfoRole <- maybe (Left $ "invalid role: " <> T.unpack roleText) Right $ injFrom roleText
  let mucInfoRealJid = getAttr "jid" item >>= (either (const Nothing) Just . xmppAddress) >>= fullJidGet
  return MUCInfo {..}

data PendingMUC m = PendingMUC
  { pmucMembers :: Map XMPPResource (PresenceRef m)
  , pmucNick :: XMPPResource
  , pmucSlot :: MUCRoomSlot m
  , pmucOnJoined :: MUCJoinResult m -> m ()
  }

data MUCRef m = MUCRef
  { mucRoom :: MUC m
  , mucSlot :: MUCRoomSlot m
  , mucState :: RegistryRef Unconstrained
  {- ^ Mutable per-room registry. Created when the join is initiated and
  preserved for the lifetime of the room (dropped when the room is left).
  Holds a 'DiscoNodeCache' that backs all disco-info lookups for the room.
  -}
  }

-- | Hand-written 'Show' that omits the per-room slot.
instance Show (MUCRef m) where
  showsPrec p MUCRef {mucRoom} =
    showParen (p > 10) $
      showString "MUCRef {mucRoom = " . showsPrec 0 mucRoom . showString "}"

type MUCSlot m = Slot m (MUCEvent m)

data MUCPlugin m = MUCPlugin
  { mucPluginSession :: StanzaSession m
  , mucPluginDisco :: DiscoPlugin m
  , mucPluginPresence :: PresencePlugin m
  , mucPluginSlot :: MUCSlot m
  , mucPluginRooms :: IORef (Map BareJID (Either (PendingMUC m) (MUCRef m)))
  }

mucNS :: Text
mucName :: Text -> Name
(mucNS, mucName) = namePair "http://jabber.org/protocol/muc"

instance (Typeable m) => DiscoInfoProvider (MUCPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton mucNS

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

{- | Look up the server-suggested registered nickname for a MUC room
(XEP-0045 §7.2.16, via the @x-roomuser-item@ disco node). Fires with
@Right (Just nick)@ when the server proposes a usable resource;
@Right Nothing@ when the disco response carries no nick identity, or when
the room/service responds @\<feature-not-implemented/\>@ (per XEP-0045
§7.2.16 the node is OPTIONAL); @Left err@ on other disco failures or when
the proposed name fails to parse as an 'XMPPResource'.
-}
mucGetRegisteredNick :: (MonadStream m) => MUCPlugin m -> BareJID -> (Either StanzaError (Maybe XMPPResource) -> m ()) -> m ()
mucGetRegisteredNick (MUCPlugin {mucPluginDisco}) room handler =
  getDiscoEntity mucPluginDisco room (Just mucNickNode) $ \nickResp ->
    handler $ case nickResp of
      Left (StanzaError {szeCondition = ScFeatureNotImplemented}) -> Right Nothing
      Left err -> Left err
      Right ent -> case M.lookup mucNickIdentity $ discoIdentities ent of
        Just (Just n) -> case resourceFromText $ localizedGet Nothing n of
          Just r -> Right (Just r)
          Nothing -> Left $ badRequest "invalid resource name proposed by server"
        _ -> Right Nothing

{- | Join a MUC room. The join handler is invoked with a 'MUCJoinFinished'
carrying the 'MUCRef' once the room is fully joined; subscribe to
'mucSlot' from there if you want to receive subsequent room events. The
handler runs synchronously between the rooms-map transition and the next
incoming stanza, so no per-room events can be missed.
-}
mucJoin :: forall m. (MonadStream m) => MUCPlugin m -> FullJID -> MUCJoinSettings -> (MUCJoinResult m -> m ()) -> m ()
mucJoin MUCPlugin {..} addr (MUCJoinSettings {joinHistory = MUCHistorySettings {..}, ..}) pmucOnJoined = do
  pmucSlot <- Slot.new
  let initialRoom =
        PendingMUC
          { pmucNick = fullResource addr
          , pmucMembers = M.empty
          , pmucSlot = pmucSlot
          , ..
          }
      roomAddr = fullBare addr
      takeRoom = do
        good <- atomicModifyIORef' mucPluginRooms $ \rooms ->
          case M.lookup roomAddr rooms of
            Nothing -> (M.insert roomAddr (Left initialRoom) rooms, True)
            Just _ -> (rooms, False)
        unless good $ throwM MUCAlreadyJoinedError
      cleanupRoom = atomicModifyIORef' mucPluginRooms $ \rooms -> (M.delete roomAddr rooms, ())
      historyAttrs =
        catMaybes
          [ fmap (\n -> ("maxchars", showt n)) histMaxChars
          , fmap (\n -> ("maxstanzas", showt n)) histMaxStanzas
          , fmap (\n -> ("seconds", showt n)) histSeconds
          , fmap (\t -> ("since", utcTimeToXmpp t)) histSince
          ]
      xElement =
        element
          (mucName "x")
          []
          [ NodeElement $ element (mucName "history") historyAttrs []
          ]
      sendRequest = do
        result <- presenceStanza mucPluginPresence $ Just joinPresence {presenceRaw = xElement : presenceRaw joinPresence}
        case result of
          Left err -> do
            cleanupRoom
            pmucOnJoined $ MUCJoinError err
          Right presStanza ->
            void $
              stanzaSend mucPluginSession $
                presStanza
                  { ostTo = Just $ toXMPPAddress addr
                  }
  bracketOnError takeRoom (const cleanupRoom) (const sendRequest)

data MUCAlreadyLeftError = MUCAlreadyLeftError
  deriving (Show, Typeable)

instance Exception MUCAlreadyLeftError

mucSendPresence :: forall m. (MonadStream m) => MUCPlugin m -> BareJID -> Maybe Presence -> m (Either StanzaError ())
mucSendPresence MUCPlugin {..} addr pres = do
  rooms <- readIORef mucPluginRooms
  case M.lookup addr rooms of
    Just (Right MUCRef {mucRoom = room}) -> do
      result <- presenceStanza mucPluginPresence pres
      case result of
        Left err -> return $ Left err
        Right presStanza -> do
          _ <-
            stanzaSend mucPluginSession $
              presStanza
                { ostTo = Just $ toXMPPAddress $ FullJID addr (mucNick room)
                }
          return $ Right ()
    _ -> throwM MUCAlreadyLeftError

instance (MonadStream m) => Handler m InStanza InResponse (MUCPlugin m) where
  tryHandle (MUCPlugin {..}) (InStanza {istFrom = Just (fullJidGet -> Just addr), istType = InPresence (Just PresenceError), istChildren}) = do
    let err = either id id $ parseStanzaError istChildren
    mpromise <- atomicModifyIORef' mucPluginRooms $ \rooms ->
      case M.lookup (fullBare addr) rooms of
        Just (Left pending) | pmucNick pending == fullResource addr -> (M.delete (fullBare addr) rooms, Just $ pmucOnJoined pending)
        _ -> (rooms, Nothing)
    case mpromise of
      Nothing -> return Nothing
      Just promise -> do
        Slot.call mucPluginSlot $ MUCRejected addr err
        promise (MUCJoinError err)
        return $ Just InSilent
  tryHandle (MUCPlugin {..}) (InStanza {istFrom = Just addr@(bareJidGet -> Just bare), istType = InMessage MessageGroupchat, istChildren}) =
    let cur = fromChildren istChildren
        mSubjE = listToMaybe $ cur $/ XC.element (jcName "subject") &| curElement
        mXE = listToMaybe $ cur $/ XC.element (mucUserName "x") &| curElement
     in case mSubjE of
          Just subjE -> do
            rooms <- readIORef mucPluginRooms
            case M.lookup bare rooms of
              Just (Right ref@MUCRef {mucRoom = room, mucSlot = slot}) -> do
                let subj = (,mconcat $ fromElement subjE $/ content) <$> addressResource addr
                    room' = room {mucSubject = subj}
                    ref' = ref {mucRoom = room'}
                atomicWriteIORef mucPluginRooms $ M.insert bare (Right ref') rooms
                Slot.call mucPluginSlot $ MUCRoomEvent bare ref' RoomSubject
                Slot.call slot (ref', RoomSubject)
                return $ Just InSilent
              _ -> return Nothing
          Nothing -> case mXE of
            Just xE
              | let statuses = parseStatusCodes xE
              , not (S.null statuses) -> do
                  rooms <- readIORef mucPluginRooms
                  case M.lookup bare rooms of
                    Just (Right ref@MUCRef {mucSlot = slot}) -> do
                      let ev = RoomConfigChanged statuses
                      Slot.call mucPluginSlot $ MUCRoomEvent bare ref ev
                      Slot.call slot (ref, ev)
                      return $ Just InSilent
                    _ -> return Nothing
            _ -> return Nothing
  tryHandle _ _ = return Nothing

data MUCHandleResult a
  = NotMUCEvent
  | MUCHandled
  | MUCRun a

instance (MonadStream m) => Handler m (PresenceUpdate m) () (MUCPlugin m) where
  tryHandle _ (AllResourcesOffline _ _) = return Nothing -- MUC rooms don't use bare JID presence
  tryHandle (MUCPlugin {..}) (ResourcePresence addr mpres) = do
    let bare = fullBare addr
        resource = fullResource addr
    processed <- atomicModifyIORef' mucPluginRooms $ \rooms ->
      case M.lookup bare rooms of
        Just (Right MUCRef {mucRoom = MUC {..}})
          | ResourceUnavailable _ <- mpres
          , resource == mucNick ->
              (M.delete bare rooms, MUCRun $ Slot.call mucPluginSlot $ MUCLeftRoom addr MUCLeft)
        Just (Right ref@MUCRef {mucRoom = room@(MUC {..}), mucSlot = slot}) ->
          case presenceUpdate resource mpres mucMembers of
            Nothing -> (rooms, MUCHandled)
            Just (members, event) ->
              let room' = room {mucMembers = members}
                  ref' = ref {mucRoom = room'}
                  roomEvent = case event of
                    Added _ presRef -> MUCJoined presRef
                    Updated _ presRef -> MUCUpdated presRef
                    Removed _ _ -> MUCRemoved MUCLeft
                  ev = RoomPresence resource roomEvent
                  fire = do
                    Slot.call mucPluginSlot $ MUCRoomEvent bare ref' ev
                    Slot.call slot (ref', ev)
               in (M.insert bare (Right ref') rooms, MUCRun fire)
        Just (Left pending) ->
          case mpres of
            ResourceAvailable presRef ->
              let members = M.insert resource presRef $ pmucMembers pending
                  pending' = pending {pmucMembers = members}
                  room' =
                    MUC
                      { mucMembers = members
                      , mucSubject = Nothing
                      , mucNick = pmucNick pending
                      , mucNonAnonymous = False
                      }
                  joinRoom = do
                    state <- RegRef.new
                    let ref = MUCRef {mucRoom = room', mucSlot = pmucSlot pending, mucState = state}
                    atomicModifyIORef' mucPluginRooms $ \rs -> (M.insert bare (Right ref) rs, ())
                    Slot.call mucPluginSlot $ MUCJoinedRoom addr ref
                    pmucOnJoined pending $ MUCJoinFinished ref
               in if resource == pmucNick pending
                    then (rooms, MUCRun joinRoom)
                    else (M.insert bare (Left pending') rooms, MUCHandled)
            ResourceUnavailable err
              | resource == pmucNick pending ->
                  let leaveRoom = do
                        Slot.call mucPluginSlot $ MUCLeftRoom addr MUCLeft
                        pmucOnJoined pending $ MUCJoinStopped err
                   in (M.delete bare rooms, MUCRun leaveRoom)
            _ -> (rooms, MUCHandled)
        Nothing -> (rooms, NotMUCEvent)
    case processed of
      MUCRun action -> do
        action
        return $ Just ()
      MUCHandled -> return $ Just ()
      NotMUCEvent -> return Nothing

getMUCPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (MUCPlugin m)
getMUCPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (MUCPlugin m)) $ pluginsHooksSet pluginsRef

{- | Look up a joined room by its bare JID. Returns 'Nothing' if the room is
not yet joined (still pending) or unknown.
-}
lookupMUCRoom :: (MonadStream m) => MUCPlugin m -> BareJID -> m (Maybe (MUCRef m))
lookupMUCRoom MUCPlugin {mucPluginRooms} bare = do
  rooms <- readIORef mucPluginRooms
  return $ case M.lookup bare rooms of
    Just (Right ref) -> Just ref
    _ -> Nothing

mucPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
mucPlugin pluginsRef = do
  mucPluginRooms <- newIORef M.empty
  mucPluginSlot <- Slot.new
  mucPluginDisco <- getDiscoPlugin pluginsRef
  mucPluginPresence <- getPresencePlugin pluginsRef
  let mucPluginSession = pluginsSession pluginsRef
      plugin :: MUCPlugin m = MUCPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin $ presencePluginHandlers mucPluginPresence
  Filter.pushNewOrFailM MUCPresenceFilter $ presencePluginReceiveFilters mucPluginPresence
  addDiscoInfo pluginsRef plugin
