{-# LANGUAGE Strict #-}

module Network.XMPP.Presence (
  ShowState (..),
  ResourceStatus (..),
  PresenceUpdate (..),
  PresenceHandler,
  Presence (..),
  defaultPresence,
  presenceHandlers,
  presencePlugin,
  PresenceEvent (..),
  presenceUpdate,
  presenceStanza,
)
where

import Control.HandlerList (HandlerList)
import qualified Control.HandlerList as HandlerList
import Control.Monad
import Control.Monad.Logger
import Data.Injective
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)

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
  , presenceExtended :: [Element]
  }
  deriving (Show, Eq)

defaultPresence :: Presence
defaultPresence =
  Presence
    { presenceShow = Nothing
    , presenceStatus = Nothing
    , presencePriority = 0
    , presenceExtended = []
    }

-- | Status of a single resource.
data ResourceStatus
  = ResourceAvailable Presence
  | -- | Resource went unavailable; carries extended stanza elements.
    ResourceUnavailable [Element]
  deriving (Show, Eq)

{- | A presence update from a specific resource, or a bare-JID unavailable
notification meaning all resources are offline (RFC 6121 §4.5.4).
-}
data PresenceUpdate
  = ResourcePresence FullJID ResourceStatus
  | -- | All resources offline; carries the bare JID and extended stanza elements.
    AllResourcesOffline BareJID [Element]
  deriving (Show, Eq)

type PresenceHandler m = PresenceUpdate -> m (Maybe ())

data PresenceOp
  = PresenceSet
  | PresenceUnset
  deriving (Show, Eq)

presenceOp :: Maybe PresenceType -> Maybe PresenceOp
presenceOp Nothing = Just PresenceSet
presenceOp (Just PresenceUnavailable) = Just PresenceUnset
presenceOp _ = Nothing

presenceHandlers :: (MonadStream m) => XMPPPluginsRef m -> m (HandlerList m PresenceUpdate ())
presenceHandlers = getPluginsHook Proxy

emitPresence :: (MonadStream m) => HandlerList m PresenceUpdate () -> PresenceUpdate -> m ()
emitPresence handlers upd = do
  mr <- HandlerList.call handlers upd
  case mr of
    Just () -> return ()
    Nothing -> $(logWarn) [i|Unhandled presence update: #{upd}|]

parsePresence :: [Element] -> Either StanzaError Presence
parsePresence elems = do
  let cur = fromChildren elems

  presenceShow <- case cur $/ XC.element (jcName "show") &/ content of
    [val] -> case injFrom val of
      Just sh -> return $ Just sh
      Nothing -> Left $ badRequest "parsePresence: invalid show"
    [] -> return Nothing
    _ -> Left $ badRequest "parsePresence: multiple show values"
  presenceStatus <- sequence $ localizedFromElement (jcName "status") elems
  presencePriority <- case cur $/ XC.element (jcName "priority") &/ content of
    [val] -> case readIntMaybe $ T.unpack val of
      Nothing -> Left $ badRequest "parsePresence: invalid priority value"
      Just r -> return r
    [] -> return 0
    _ -> Left $ badRequest "parsePresence: multiple priority values"
  let presenceExtended = parseExtended elems

  return Presence {..}

parseExtended :: [Element] -> [Element]
parseExtended elems = fromChildren elems $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement

presenceInHandler :: (MonadStream m) => HandlerList m PresenceUpdate () -> PluginInHandler m
presenceInHandler handlers (InStanza {istType = InPresence (Right (presenceOp -> Just op)), istFrom = Just (fullJidGet -> Just faddr), istChildren}) =
  Just <$> do
    case op of
      PresenceSet -> case parsePresence istChildren of
        Right p -> do
          emitPresence handlers $ ResourcePresence faddr $ ResourceAvailable p
          return InSilent
        Left e -> return $ InError e
      PresenceUnset -> do
        emitPresence handlers $ ResourcePresence faddr $ ResourceUnavailable $ parseExtended istChildren
        return InSilent
-- Bare-JID unavailable presence (RFC 6121 §4.5.4): all resources are offline.
presenceInHandler handlers (InStanza {istType = InPresence (Right (Just PresenceUnavailable)), istFrom = Just (bareJidGet -> Just bare), istChildren}) =
  Just <$> do
    emitPresence handlers $ AllResourcesOffline bare (parseExtended istChildren)
    return InSilent
presenceInHandler _ _ = return Nothing

presencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
presencePlugin pluginsRef = do
  handlers <- HandlerList.new
  insertPluginsHook handlers pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  void $ HandlerList.add inHandlers (presenceInHandler handlers)

data PresenceEvent k
  = Added k Presence
  | Updated k Presence
  | Removed k [Element]
  deriving (Show, Eq)

presenceUpdate :: (Ord k) => k -> ResourceStatus -> Map k Presence -> Maybe (Map k Presence, PresenceEvent k)
presenceUpdate k (ResourceAvailable v) m
  | M.member k m = Just (M.insert k v m, Updated k v)
  | otherwise = Just (M.insert k v m, Added k v)
presenceUpdate k (ResourceUnavailable e) m
  | M.member k m = Just (M.delete k m, Removed k e)
  | otherwise = Nothing

presenceStanza :: Maybe Presence -> OutStanza
presenceStanza (Just (Presence {..})) =
  OutStanza
    { ostTo = Nothing
    , ostType = OutPresence Nothing
    , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceExtended
    }
 where
  priority = element (jcName "priority") [] [NodeContent $ showt presencePriority]
  mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
  statuses = maybe [] (localizedElements $ jcName "status") presenceStatus
presenceStanza Nothing =
  OutStanza
    { ostTo = Nothing
    , ostType = OutPresence (Just PresenceUnavailable)
    , ostChildren = []
    }
