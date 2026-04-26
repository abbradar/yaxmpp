{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.MUC (
  MUCEvent (..),
  RoomEvent (..),
  MUCHandler,
  MUCRole (..),
  MUCAffiliation (..),
  MUCLeaveReason (..),
  MUCPresenceEvent (..),
  MUCStatusSet,
  MUCInfo (..),
  MUCPresenceCodec (..),
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

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Injective
import Data.List (partition)
import qualified Data.Registry as Reg
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
  , mucMembers :: Map XMPPResource PresenceRef
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
  = MUCJoined PresenceRef
  | MUCUpdated PresenceRef
  | MUCRemoved MUCLeaveReason
  | MUCRenamed XMPPResource
  deriving (Show)

type MUCStatusSet = Set Integer

-- | MUC-specific presence information stored in 'presenceExtended' by 'MUCPresenceCodec'.
data MUCInfo = MUCInfo
  { mucInfoStatus :: MUCStatusSet
  , mucInfoRealJid :: Maybe FullJID
  , mucInfoAffiliation :: MUCAffiliation
  , mucInfoRole :: MUCRole
  }
  deriving (Show, Typeable)

data MUCPresenceCodec = MUCPresenceCodec

instance (MonadStream m) => Codec m FullJID Presence MUCPresenceCodec where
  codecDecode _ _ pres = case extractMUCInfo (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0045 MUC presence: #{err}|]
      return pres
    Right (mInfo, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\info -> Reg.insert info ext) mInfo
       in return $ pres {presenceRaw = raw', presenceExtended = ext'}
  codecEncode _ _ pres =
    let ext = presenceExtended pres
        (mInfo, ext') = Reg.pop (Proxy :: Proxy MUCInfo) ext
        raw = presenceRaw pres
        raw' = maybe raw (\info -> mucInfoToElement info : raw) mInfo
     in return $ pres {presenceRaw = raw', presenceExtended = ext'}

extractMUCInfo :: [Element] -> Either String (Maybe MUCInfo, [Element])
extractMUCInfo elems =
  let (xElems, rest) = partition (\e -> elementName e == mucUserName "x") elems
   in case xElems of
        [] -> Right (Nothing, rest)
        (xE : _) -> (\info -> (Just info, rest)) <$> parseMUCInfo xE

parseMUCInfo :: Element -> Either String MUCInfo
parseMUCInfo xE = do
  let mucInfoStatus = S.fromList $ mapMaybe (readMaybe . T.unpack) $ fromElement xE $/ XC.element (mucUserName "status") &/ attribute "code"
  item <- maybe (Left "missing <item> in <x>") Right $ listToMaybe $ fromElement xE $/ XC.element (mucUserName "item") &| curElement
  affText <- maybe (Left "missing affiliation attribute") Right $ getAttr "affiliation" item
  mucInfoAffiliation <- maybe (Left $ "invalid affiliation: " <> T.unpack affText) Right $ injFrom affText
  roleText <- maybe (Left "missing role attribute") Right $ getAttr "role" item
  mucInfoRole <- maybe (Left $ "invalid role: " <> T.unpack roleText) Right $ injFrom roleText
  let mucInfoRealJid = getAttr "jid" item >>= (either (const Nothing) Just . xmppAddress) >>= fullJidGet
  return MUCInfo {..}

mucInfoToElement :: MUCInfo -> Element
mucInfoToElement (MUCInfo {..}) =
  element (mucUserName "x") [] $
    map statusNode (S.toAscList mucInfoStatus)
      ++ [NodeElement itemElem]
 where
  statusNode s = NodeElement $ element (mucUserName "status") [("code", T.pack (show s))] []
  itemAttrs =
    [ ("affiliation", injTo mucInfoAffiliation)
    , ("role", injTo mucInfoRole)
    ]
      ++ maybe [] (\j -> [("jid", addressToText j)]) mucInfoRealJid
  itemElem = element (mucUserName "item") itemAttrs []

data PendingMUC m = PendingMUC
  { pmucMembers :: Map XMPPResource PresenceRef
  , pmucNick :: XMPPResource
  , pmucHandler :: MUCHandler m
  , pmucOnJoined :: MUCJoinResult -> m ()
  }

data MUCRef m = MUCRef
  { mucRoom :: MUC
  , mucHandler :: MUCHandler m
  }

type MUCSlot m = Slot m MUCEvent

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

mucJoin :: forall m. (MonadStream m) => MUCPlugin m -> FullJID -> MUCJoinSettings -> MUCHandler m -> (MUCJoinResult -> m ()) -> m ()
mucJoin MUCPlugin {..} addr (MUCJoinSettings {joinHistory = MUCHistorySettings {..}, ..}) handler pmucOnJoined = do
  let initialRoom =
        PendingMUC
          { pmucNick = fullResource addr
          , pmucMembers = M.empty
          , pmucHandler = handler
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
      joinRoom = do
        presStanza <- presenceStanza mucPluginPresence $ Just joinPresence {presenceRaw = xElement : presenceRaw joinPresence}
        void $
          stanzaSend mucPluginSession $
            presStanza
              { ostTo = Just $ toXMPPAddress addr
              }
  bracketOnError takeRoom (const cleanupRoom) (const joinRoom)

data MUCAlreadyLeftError = MUCAlreadyLeftError
  deriving (Show, Typeable)

instance Exception MUCAlreadyLeftError

mucSendPresence :: forall m. (MonadStream m) => MUCPlugin m -> BareJID -> Maybe Presence -> m ()
mucSendPresence MUCPlugin {..} addr pres = do
  rooms <- readIORef mucPluginRooms
  case M.lookup addr rooms of
    Just (Right MUCRef {mucRoom = room}) -> do
      presStanza <- presenceStanza mucPluginPresence pres
      _ <-
        stanzaSend mucPluginSession $
          presStanza
            { ostTo = Just $ toXMPPAddress $ FullJID addr (mucNick room)
            }
      return ()
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
    case fromChildren istChildren $/ XC.element (jcName "subject") &| curElement of
      (subjE : _) -> do
        rooms <- readIORef mucPluginRooms
        case M.lookup bare rooms of
          Just (Right ref@MUCRef {mucRoom = room, mucHandler = handler}) -> do
            let subj = (,mconcat $ fromElement subjE $/ content) <$> addressResource addr
                room' = room {mucSubject = subj}
            atomicWriteIORef mucPluginRooms $ M.insert bare (Right ref {mucRoom = room'}) rooms
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
    let mucEventHandler = mucPluginSlot
        bare = fullBare addr
        resource = fullResource addr
    processed <- atomicModifyIORef' mucPluginRooms $ \rooms ->
      case M.lookup bare rooms of
        Just (Right MUCRef {mucRoom = MUC {..}})
          | ResourceUnavailable _ <- mpres
          , resource == mucNick ->
              (M.delete bare rooms, MUCRun $ Slot.call mucEventHandler $ MUCLeftRoom addr MUCLeft)
        Just (Right ref@MUCRef {mucRoom = room@(MUC {..}), mucHandler = handler}) ->
          case presenceUpdate resource mpres mucMembers of
            Nothing -> (rooms, MUCHandled)
            Just (members, event) ->
              let room' = room {mucMembers = members}
                  roomEvent = case event of
                    Added _ presRef -> MUCJoined presRef
                    Updated _ presRef -> MUCUpdated presRef
                    Removed _ _ -> MUCRemoved MUCLeft
               in (M.insert bare (Right ref {mucRoom = room'}) rooms, MUCRun $ handler room' $ RoomPresence resource roomEvent)
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
                  ref = MUCRef {mucRoom = room', mucHandler = pmucHandler pending}
                  joinRoom = do
                    Slot.call mucEventHandler $ MUCJoinedRoom addr room'
                    pmucOnJoined pending (MUCJoinFinished room')
               in if resource == pmucNick pending
                    then (M.insert bare (Right ref) rooms, MUCRun joinRoom)
                    else (M.insert bare (Left pending') rooms, MUCHandled)
            ResourceUnavailable err
              | resource == pmucNick pending ->
                  let leaveRoom = do
                        Slot.call mucEventHandler $ MUCLeftRoom addr MUCLeft
                        pmucOnJoined pending (MUCJoinStopped err)
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
  Codec.pushNewOrFailM MUCPresenceCodec $ presencePluginCodecs mucPluginPresence
  addDiscoInfo pluginsRef plugin
