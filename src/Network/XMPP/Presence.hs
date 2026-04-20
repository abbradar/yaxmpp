{-# LANGUAGE Strict #-}

module Network.XMPP.Presence (
  ShowState (..),
  ResourceStatus (..),
  PresenceUpdate (..),
  PresenceHandler,
  Presence (..),
  defaultPresence,
  PresenceCodecList,
  PresencePlugin,
  presencePluginHandlers,
  presencePluginCodecs,
  getPresencePlugin,
  presencePlugin,
  PresenceEvent (..),
  presenceUpdate,
  presenceStanza,
  AllPresencesMap,
  getAllPresences,
)
where

import Control.Codec (CodecList, decodeAll, encodeAll)
import qualified Control.Codec as Codec
import Control.HandlerList (Handler (..), HandlerList)
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Monad.Logger
import Data.Injective
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Registry (Registry)
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Session (sessionAddress)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)
import UnliftIO.IORef

data ShowState
  = ShowAway
  | ShowChat
  | ShowDnD
  | ShowXA
  deriving (Show, Eq, Bounded, Enum)

instance Injective ShowState Text where
  injTo x = case x of
    ShowAway -> "away"
    ShowChat -> "chat"
    ShowDnD -> "dnd"
    ShowXA -> "xa"

data Presence = Presence
  { presenceShow :: Maybe ShowState
  , presenceStatus :: Maybe LocalizedText
  , presencePriority :: Int8
  , presenceRaw :: [Element]
  , presenceExtended :: Registry Show
  }
  deriving (Show)

defaultPresence :: Presence
defaultPresence =
  Presence
    { presenceShow = Nothing
    , presenceStatus = Nothing
    , presencePriority = 0
    , presenceRaw = []
    , presenceExtended = Reg.empty
    }

-- | Status of a single resource.
data ResourceStatus
  = ResourceAvailable Presence
  | -- | Resource went unavailable; carries extended stanza elements.
    ResourceUnavailable [Element]
  deriving (Show)

{- | A presence update from a specific resource, or a bare-JID unavailable
notification meaning all resources are offline (RFC 6121 §4.5.4).
-}
data PresenceUpdate
  = ResourcePresence FullJID ResourceStatus
  | -- | All resources offline; carries the bare JID and extended stanza elements.
    AllResourcesOffline BareJID [Element]
  deriving (Show)

type PresenceHandler m = PresenceUpdate -> m (Maybe ())

data PresenceOp
  = PresenceSet
  | PresenceUnset
  deriving (Show, Eq)

presenceOp :: Maybe PresenceType -> Maybe PresenceOp
presenceOp Nothing = Just PresenceSet
presenceOp (Just PresenceUnavailable) = Just PresenceUnset
presenceOp _ = Nothing

emitPresence :: (MonadStream m) => HandlerList m PresenceUpdate () -> PresenceUpdate -> m ()
emitPresence handlers upd = do
  mr <- HL.call handlers upd
  case mr of
    Just () -> return ()
    Nothing -> $(logWarn) [i|Unhandled presence update: #{upd}|]

parsePresence :: [Element] -> Either StanzaError Presence
parsePresence elems = do
  let cur = fromChildren elems

  presenceShow <- case cur $/ XC.element (jcName "show") &/ content of
    [val] -> case injFrom val of
      Just sh -> return $ Just sh
      Nothing -> Left $ badRequest "invalid <show> value"
    [] -> return Nothing
    _ -> Left $ badRequest "multiple <show> values"
  presenceStatus <- localizedFromElement (jcName "status") elems
  presencePriority <- case cur $/ XC.element (jcName "priority") &/ content of
    [val] -> case readIntMaybe $ T.unpack val of
      Nothing -> Left $ badRequest "invalid <priority> value"
      Just r -> return r
    [] -> return 0
    _ -> Left $ badRequest "multiple <priority> values"
  let presenceRaw = parseExtended elems
      presenceExtended = Reg.empty

  return Presence {..}

parseExtended :: [Element] -> [Element]
parseExtended elems = fromChildren elems $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement

type PresenceCodecList m = CodecList m FullJID Presence

type AllPresencesMap = Map FullJID Presence

data PresencePlugin m = PresencePlugin
  { presencePluginSession :: StanzaSession m
  , presencePluginHandlers :: HandlerList m PresenceUpdate ()
  , presencePluginCodecs :: PresenceCodecList m
  , presencePluginPresences :: IORef AllPresencesMap
  }

instance (MonadStream m) => Handler m InStanza InResponse (PresencePlugin m) where
  tryHandle (PresencePlugin {..}) (InStanza {istType = InPresence (Right (presenceOp -> Just op)), istFrom = Just (fullJidGet -> Just faddr), istChildren}) =
    Just <$> do
      case op of
        PresenceSet -> case parsePresence istChildren of
          Right p -> do
            p' <- decodeAll presencePluginCodecs faddr p
            atomicModifyIORef' presencePluginPresences . ((,()) .) $ M.insert faddr p'
            emitPresence presencePluginHandlers $ ResourcePresence faddr $ ResourceAvailable p'
            return InSilent
          Left e -> return $ InError e
        PresenceUnset -> do
          let extended = parseExtended istChildren
          atomicModifyIORef' presencePluginPresences . ((,()) .) $ M.delete faddr
          emitPresence presencePluginHandlers $ ResourcePresence faddr $ ResourceUnavailable extended
          return InSilent
  -- Bare-JID unavailable presence (RFC 6121 §4.5.4): all resources are offline.
  tryHandle (PresencePlugin {..}) (InStanza {istType = InPresence (Right (Just PresenceUnavailable)), istFrom = Just (bareJidGet -> Just bare), istChildren}) =
    Just <$> do
      let extended = parseExtended istChildren
      atomicModifyIORef' presencePluginPresences . ((,()) .) $ M.filterWithKey (\k _ -> fullBare k /= bare)
      emitPresence presencePluginHandlers $ AllResourcesOffline bare extended
      return InSilent
  tryHandle _ _ = return Nothing

-- | Get the presence plugin from the plugins hook set.
getPresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (PresencePlugin m)
getPresencePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (PresencePlugin m)) $ pluginsHooksSet pluginsRef

-- | Get the map of all currently available presences.
getAllPresences :: (MonadStream m) => PresencePlugin m -> m AllPresencesMap
getAllPresences PresencePlugin {presencePluginPresences} = readIORef presencePluginPresences

presencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
presencePlugin pluginsRef = do
  presencePluginHandlers <- HL.new
  presencePluginCodecs <- Codec.new
  presencePluginPresences <- newIORef M.empty
  let presencePluginSession = pluginsSession pluginsRef
      plugin :: PresencePlugin m = PresencePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers

data PresenceEvent k
  = Added k Presence
  | Updated k Presence
  | Removed k [Element]
  deriving (Show)

presenceUpdate :: (Ord k) => k -> ResourceStatus -> Map k Presence -> Maybe (Map k Presence, PresenceEvent k)
presenceUpdate k (ResourceAvailable v) m
  | M.member k m = Just (M.insert k v m, Updated k v)
  | otherwise = Just (M.insert k v m, Added k v)
presenceUpdate k (ResourceUnavailable e) m
  | M.member k m = Just (M.delete k m, Removed k e)
  | otherwise = Nothing

presenceStanza :: (MonadStream m) => PresencePlugin m -> Maybe Presence -> m OutStanza
presenceStanza PresencePlugin {presencePluginSession, presencePluginCodecs} (Just pres) = do
  let myAddr = sessionAddress $ ssSession presencePluginSession
  Presence {..} <- encodeAll presencePluginCodecs myAddr pres
  unless (Reg.null presenceExtended) $ error "presenceStanza: presenceExtended is not empty after encoding"
  let priority = element (jcName "priority") [] [NodeContent $ showt presencePriority]
      mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
      statuses = maybe [] (localizedElements $ jcName "status") presenceStatus
  return
    OutStanza
      { ostTo = Nothing
      , ostType = OutPresence Nothing
      , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceRaw
      }
presenceStanza _ Nothing =
  return
    OutStanza
      { ostTo = Nothing
      , ostType = OutPresence (Just PresenceUnavailable)
      , ostChildren = []
      }
