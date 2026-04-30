{-# LANGUAGE Strict #-}

module Network.XMPP.Roster (
  SubscriptionType (..),
  RosterEntry (..),
  RosterEntries,
  Roster (..),
  RosterVersion (..),
  rosterName,
  parseRosterEntry,
  RosterEvent (..),
  RosterSlot,
  RosterPlugin,
  rosterPluginSession,
  rosterPluginSlot,
  getRosterPlugin,
  getRoster,
  tryGetRoster,
  insertRoster,
  deleteRoster,
  rosterPlugin,
) where

import Control.Monad
import Control.Monad.Logger
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import UnliftIO.Concurrent
import UnliftIO.Exception (SomeException, catch)
import UnliftIO.IORef

import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Injective
import qualified Data.Registry.Mutable as RegRef
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Session (sessionStreamFeatures)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML

data SubscriptionType
  = SubNone
  | SubFrom
  | SubTo
  | SubBoth
  deriving (Show, Eq, Enum, Bounded)

instance Injective SubscriptionType Text where
  injTo x = case x of
    SubNone -> "none"
    SubFrom -> "from"
    SubTo -> "to"
    SubBoth -> "both"

instance FromJSON SubscriptionType where
  parseJSON = injParseJSON "SubscriptionType"

instance ToJSON SubscriptionType where
  toJSON = injToJSON

type RosterName = Text
type RosterGroup = Text

data RosterEntry = RosterEntry
  { rentryName :: Maybe RosterName
  , rentrySubscription :: SubscriptionType
  , rentrySubscriptionAsked :: Bool
  , rentryGroups :: Set RosterGroup
  }
  deriving (Show, Eq, Generic)

instance FromJSON RosterEntry
instance ToJSON RosterEntry

type RosterEntries = Map XMPPAddress RosterEntry

data Roster = Roster
  { rosters :: RosterEntries
  , rosterVersion :: Maybe RosterVersion
  }
  deriving (Show)

-- | The @ver@ attribute reported by the server (RFC 6121 §2.6).
newtype RosterVersion = RosterVersion {rosterVersionText :: Text}
  deriving (Show, Eq)

-- | On-disk shape: only versioned rosters are persisted.
data RosterCache = RosterCache
  { rcVersion :: Text
  , rcEntries :: RosterEntries
  }
  deriving (Show, Eq, Generic)

instance FromJSON RosterCache
instance ToJSON RosterCache

rosterName :: Text -> Name
rosterName = nsName "jabber:iq:roster"

rosterVerName :: Text -> Name
rosterVerName = nsName "urn:xmpp:features:rosterver"

isRosterVer :: Element -> Bool
isRosterVer e = elementName e == rosterVerName "ver"

rosterverSupported :: StanzaSession m -> Bool
rosterverSupported session = any isRosterVer $ sessionStreamFeatures $ ssSession session

okHandler :: (MonadStream m) => IQResponseHandler m
okHandler (Left err) = $(logError) [i|okHandler: error while updating a roster: #{err}|]
okHandler (Right _) = return ()

data RosterEvent
  = RosterInsert XMPPAddress RosterEntry
  | RosterDelete XMPPAddress
  deriving (Show, Eq)

type RosterSlot m = Slot m (RosterEntries, RosterEvent)

{- | Memoized initial roster fetch. Once resolved, on success it holds an
'IORef' wrapping the live roster (updated in-place by subsequent pushes);
on failure it holds the 'StanzaError' that prevented obtaining it.
-}
type RosterMemo m = AsyncMemo m (Either StanzaError (IORef Roster))

data RosterPlugin m = RosterPlugin
  { rosterPluginSession :: StanzaSession m
  , rosterPluginSlot :: RosterSlot m
  , rosterPluginRef :: RosterMemo m
  , rosterPluginInitial :: IORef (Maybe RosterCache)
  {- ^ Versioned cache loaded from disk; used to send the @ver@ attribute on the
  opening request and to reuse entries when the server replies empty.
  -}
  }

{- | Persist the live roster's version (if any). A live roster without a
'RosterVersion' returns 'JSON.Null' to drop the cache: serializing a stale
@ver@ alongside fresh entries would let the server skip a needed full
resync. If the live roster hasn't loaded yet, fall back to whatever the
plugin booted with so we don't lose a still-valid disk entry.
-}
instance (MonadStream m) => XMPPPersistentCache m (RosterPlugin m) where
  cacheKey _ = "roster"
  cacheGet RosterPlugin {rosterPluginRef, rosterPluginInitial} = do
    resolved <- AsyncMemo.tryGet rosterPluginRef
    case resolved of
      Just (Right rosterRef) -> do
        roster <- readIORef rosterRef
        case rosterVersion roster of
          Just (RosterVersion ver) -> return $ JSON.toJSON $ RosterCache ver (rosters roster)
          Nothing -> return JSON.Null
      _ -> maybe JSON.Null JSON.toJSON <$> readIORef rosterPluginInitial

insertRoster :: (MonadStream m) => RosterPlugin m -> XMPPAddress -> Maybe RosterName -> Set RosterGroup -> m ()
insertRoster rp jid name groups =
  getRoster rp $ \case
    Left err -> $(logError) [i|insertRoster: roster not available: #{err}|]
    Right _ -> stanzaRequest (rosterPluginSession rp) request okHandler
 where
  request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
  item =
    element
      (rosterName "item")
      (("jid", addressToText jid) : maybeToList (fmap ("name",) name))
      $ map (\g -> NodeElement $ element (rosterName "group") [] [NodeContent g])
      $ S.toList groups

deleteRoster :: (MonadStream m) => RosterPlugin m -> XMPPAddress -> m ()
deleteRoster rp jid =
  getRoster rp $ \case
    Left err -> $(logError) [i|deleteRoster: roster not available: #{err}|]
    Right _ -> stanzaRequest (rosterPluginSession rp) request okHandler
 where
  request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
  item = element (rosterName "item") [("jid", addressToText jid), ("subscription", "remove")] []

-- | Parse a single @\<item/\>@ child of @\<query/\>@.
parseRosterEntry :: Element -> Either Text (XMPPAddress, RosterEntry)
parseRosterEntry e = do
  unless (elementName e == rosterName "item") $ Left [i|parseRosterEntry: invalid roster item #{e}|]
  jid <- case getAttr "jid" e >>= (toRight . xmppAddress) of
    Nothing -> Left "parseRosterEntry: malformed jid"
    Just r -> return r
  rentrySubscription <- case injFrom $ fromMaybe "none" $ getAttr "subscription" e of
    Nothing -> Left "parseRosterEntry: invalid subscription type"
    Just r -> return r
  rentrySubscriptionAsked <- case getAttr "ask" e of
    Nothing -> return False
    Just "subscribe" -> return True
    _ -> Left "parseRosterEntry: invalid ask type"
  let entry =
        RosterEntry
          { rentryName = getAttr "name" e
          , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
          , ..
          }
  return (jid, entry)

parseInitialQuery :: Element -> Either Text RosterEntries
parseInitialQuery res = do
  entries <- mapM parseRosterEntry $ fromElement res $/ curAnyElement
  return $ M.fromList entries

{- | Send the initial @IQGet@ for the roster, including the cached @ver@
attribute if the server advertised XEP rosterver support and we have a
cache. An empty response with a usable cache is treated as a cache hit.
-}
sendInitialRequest ::
  (MonadStream m) =>
  StanzaSession m ->
  Maybe RosterCache ->
  (Either StanzaError Roster -> m ()) ->
  m ()
sendInitialRequest session mcache callback = do
  let mver =
        if rosterverSupported session
          then Just $ maybe "" rcVersion mcache
          else Nothing
      verAttr = maybeToList $ fmap ("ver",) mver
      req = serverRequest IQGet [element (rosterName "query") verAttr []]
  stanzaRequest session req $ \resp -> case resp of
    Left err -> callback $ Left err
    Right [] | Just c <- mcache -> do
      $(logInfo) [i|Reusing cached roster (version: #{rcVersion c})|]
      callback $ Right $ Roster {rosters = rcEntries c, rosterVersion = Just (RosterVersion (rcVersion c))}
    Right [] -> callback $ Left $ badRequest "empty roster response with no cache"
    Right [res] | elementName res == rosterName "query" ->
      case parseInitialQuery res of
        Left e -> callback $ Left $ badRequest e
        Right entries ->
          callback $ Right $ Roster {rosters = entries, rosterVersion = RosterVersion <$> getAttr "ver" res}
    _ -> callback $ Left $ badRequest "invalid roster response"

parseRosterEvent :: Element -> Either StanzaError RosterEvent
parseRosterEvent e = do
  unless (elementName e == rosterName "item") $ Left $ badRequest [i|invalid roster item #{e}|]
  jid <- case getAttr "jid" e >>= (toRight . xmppAddress) of
    Nothing -> Left $ jidMalformed "invalid jid in roster push"
    Just r -> return r
  rentrySubscriptionAsked <- case getAttr "ask" e of
    Nothing -> return False
    Just "subscribe" -> return True
    _ -> Left $ badRequest "invalid ask type"
  let subscr = fromMaybe "none" (getAttr "subscription" e)
  case injFrom subscr of
    Just rentrySubscription -> do
      let entry' =
            RosterEntry
              { rentryName = getAttr "name" e
              , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
              , ..
              }
      return $ RosterInsert jid entry'
    Nothing | subscr == "remove" -> return $ RosterDelete jid
    _ -> Left $ badRequest "invalid subscription attribute"

applyRosterEvent :: RosterEntries -> RosterEvent -> RosterEntries
applyRosterEvent roster (RosterInsert jid entry) = M.insert jid entry roster
applyRosterEvent roster (RosterDelete jid) = M.delete jid roster

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (RosterPlugin m) where
  tryHandle (RosterPlugin {..}) (InRequestIQ {iriType = IQSet, iriFrom, iriChildren = [req]})
    | elementName req == rosterName "query" =
        Just <$> do
          if not (fromServerOrMyself iriFrom rosterPluginSession)
            then
              return $ IQError $ serviceUnavailable "Roster push from arbitrary entity is forbidden"
            else do
              case mapM parseRosterEvent $ fromElement req $/ curAnyElement of
                Left err -> return $ IQError err
                Right events -> do
                  resolved <- AsyncMemo.tryGet rosterPluginRef
                  case resolved of
                    Nothing -> return $ IQError $ badRequest "Roster has not been received yet"
                    Just (Left _) -> return $ IQError $ badRequest "Roster has not been received yet"
                    Just (Right rosterRef) -> do
                      revents <- atomicModifyIORef' rosterRef $ \roster ->
                        let entrySteps = drop 1 $ scanl applyRosterEvent (rosters roster) events
                            finalEntries = foldl (\_ x -> x) (rosters roster) entrySteps
                            roster' =
                              roster
                                { rosters = finalEntries
                                , rosterVersion = maybe (rosterVersion roster) (Just . RosterVersion) (getAttr "ver" req)
                                }
                         in (roster', zip entrySteps events)
                      mapM_ (Slot.call rosterPluginSlot) revents
                      return $ IQResult []
  tryHandle _ _ = return Nothing

{- | Register a callback to receive the initial roster (or the error that
prevented obtaining it). If the roster has already been resolved, the
callback is invoked immediately; otherwise it is queued and run once the
roster arrives. Subsequent roster pushes do NOT re-trigger registered
callbacks; use 'rosterPluginSlot' for live updates.
-}
getRoster :: (MonadStream m) => RosterPlugin m -> (Either StanzaError Roster -> m ()) -> m ()
getRoster RosterPlugin {rosterPluginRef} cb =
  AsyncMemo.get rosterPluginRef $ \case
    Left err -> cb (Left err)
    Right rosterRef -> do
      roster <- readIORef rosterRef
      cb $ Right roster

tryGetRoster :: (MonadStream m) => RosterPlugin m -> m (Maybe (Either StanzaError Roster))
tryGetRoster RosterPlugin {rosterPluginRef} =
  AsyncMemo.tryGet rosterPluginRef >>= \case
    Just (Right rosterRef) -> Just . Right <$> readIORef rosterRef
    Just (Left err) -> return $ Just $ Left err
    _ -> return Nothing

getRosterPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (RosterPlugin m)
getRosterPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (RosterPlugin m)) $ pluginsHooksSet pluginsRef

rosterPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
rosterPlugin pluginsRef = do
  let oldCache = pluginsOldCacheFor pluginsRef (Proxy :: Proxy (RosterPlugin m)) >>= JSON.parseMaybe parseJSON
  rosterPluginInitial <- newIORef oldCache
  rosterPluginSlot <- Slot.new
  let rosterPluginSession = pluginsSession pluginsRef
      fetchInitial done = do
        let resolveWith result = case result of
              Right roster -> do
                rosterRef <- newIORef roster
                done (Right rosterRef)
              Left err -> done (Left err)
            tryGet = sendInitialRequest rosterPluginSession oldCache resolveWith
        void $
          forkIO $
            tryGet `catch` \(e :: SomeException) ->
              resolveWith $ Left $ badRequest $ T.pack $ show e
  rosterPluginRef <- AsyncMemo.new fetchInitial
  let plugin :: RosterPlugin m = RosterPlugin {..}
  AsyncMemo.get rosterPluginRef $ \_ -> return ()
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  registerCacheGetter pluginsRef plugin
  HL.pushNewOrFailM plugin $ pluginsIQHandlers pluginsRef
