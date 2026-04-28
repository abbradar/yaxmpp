{-# LANGUAGE Strict #-}

module Network.XMPP.Roster (
  SubscriptionType (..),
  RosterEntry (..),
  RosterEntries,
  Roster,
  rosterVersion,
  rosterEntries,
  RosterEvent (..),
  RosterSlot,
  RosterPlugin,
  rosterPluginSlot,
  getRosterPlugin,
  getRoster,
  tryGetRoster,
  insertRoster,
  deleteRoster,
  rosterPlugin,
) where

import Control.Exception (SomeException, throw)
import Control.Monad
import Control.Monad.Logger
import Data.Aeson
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
import UnliftIO.Exception (catch)
import UnliftIO.IORef

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Injective
import qualified Data.Registry.Mutable as RegRef
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Session
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
  { rosterVersion :: Maybe Text
  , rosterEntries :: RosterEntries
  }
  deriving (Show, Eq, Generic)

instance FromJSON Roster
instance ToJSON Roster

rosterVerName :: Text -> Name
rosterVerName = nsName "urn:xmpp:features:rosterver"

isRosterVer :: Element -> Bool
isRosterVer e = elementName e == rosterVerName "ver"

rosterName :: Text -> Name
rosterName = nsName "jabber:iq:roster"

okHandler :: (MonadStream m) => IQResponseHandler m
okHandler (Left err) = $(logError) [i|okHandler: error while updating a roster: #{err}|]
okHandler (Right _) = return ()

data RosterEvent
  = RosterInsert XMPPAddress RosterEntry
  | RosterDelete XMPPAddress
  deriving (Show, Eq)

type RosterSlot m = Slot m (RosterEntries, RosterEvent)

data RosterPlugin m = RosterPlugin
  { rosterPluginSession :: StanzaSession m
  , rosterPluginSlot :: RosterSlot m
  , rosterPluginRef :: IORef (Either (MVar Roster) Roster)
  }

instance (MonadStream m) => XMPPPersistentCache m (RosterPlugin m) where
  cacheKey _ = "roster"
  cacheGet RosterPlugin {..} = do
    r <- readIORef rosterPluginRef
    case toRight r of
      Just roster | Just _ <- rosterVersion roster -> return $ toJSON roster
      _ -> return Null

insertRoster :: (MonadStream m) => RosterPlugin m -> XMPPAddress -> Maybe RosterName -> Set RosterGroup -> m ()
insertRoster rp jid name groups = do
  void $ getRoster rp
  stanzaRequest (rosterPluginSession rp) request okHandler
 where
  request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
  item =
    element
      (rosterName "item")
      (("jid", addressToText jid) : maybeToList (fmap ("name",) name))
      $ map (\g -> NodeElement $ element (rosterName "group") [] [NodeContent g])
      $ S.toList groups

deleteRoster :: (MonadStream m) => RosterPlugin m -> XMPPAddress -> m ()
deleteRoster rp jid = do
  void $ getRoster rp
  stanzaRequest (rosterPluginSession rp) request okHandler
 where
  request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
  item = element (rosterName "item") [("jid", addressToText jid), ("subscription", "remove")] []

parseInitial :: Element -> Either Text (XMPPAddress, RosterEntry)
parseInitial e = do
  unless (elementName e == rosterName "item") $ Left [i|parseInitial: invalid roster item #{e}|]
  jid <- case getAttr "jid" e >>= (toRight . xmppAddress) of
    Nothing -> Left "parseInitial: malformed jid"
    Just r -> return r
  rentrySubscription <- case injFrom $ fromMaybe "none" $ getAttr "subscription" e of
    Nothing -> Left "parseInitial: invalid subscription type"
    Just r -> return r
  rentrySubscriptionAsked <- case getAttr "ask" e of
    Nothing -> return False
    Just "subscribe" -> return True
    _ -> Left "parseInitial: invalid ask type"
  let entry =
        RosterEntry
          { rentryName = getAttr "name" e
          , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
          , ..
          }
  return (jid, entry)

sendFirstRequest :: (MonadStream m) => StanzaSession m -> Maybe Roster -> (Roster -> m ()) -> m ()
sendFirstRequest session mold handler = do
  let ver =
        if any isRosterVer $ sessionStreamFeatures $ ssSession session
          then Just $ fromMaybe "" $ mold >>= rosterVersion
          else Nothing
      req = serverRequest IQGet [element (rosterName "query") (maybeToList $ fmap ("ver",) ver) []]

  stanzaRequest session req $ \resp ->
    case (mold, resp) of
      (_, Left err) -> fail [i|handleFirstRequest: error while requesting roster: #{err}|]
      (Just old, Right []) | isJust $ rosterVersion old -> do
        $(logInfo) [i|Reusing cached roster (version: #{rosterVersion old})|]
        handler old
      (_, Right [res]) | elementName res == rosterName "query" ->
        case mapM parseInitial $ fromElement res $/ curAnyElement of
          Left e -> fail $ T.unpack e
          Right entries ->
            handler $
              Roster
                { rosterVersion = getAttr "ver" res
                , rosterEntries = M.fromList entries
                }
      _ -> fail "handleFirstRequest: invalid roster response"

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
                  result <- atomicModifyIORef' rosterPluginRef $ \case
                    pending@(Left _) -> (pending, Nothing)
                    Right roster ->
                      let rosters = drop 1 $ scanl applyRosterEvent (rosterEntries roster) events
                          finalEntries = foldl (\_ x -> x) (rosterEntries roster) rosters
                          roster' =
                            Roster
                              { rosterVersion = getAttr "ver" req
                              , rosterEntries = finalEntries
                              }
                       in (Right roster', Just $ zip rosters events)
                  case result of
                    Nothing -> return $ IQError $ badRequest "Roster has not been received yet"
                    Just revents -> do
                      mapM_ (Slot.call rosterPluginSlot) revents
                      return $ IQResult []
  tryHandle _ _ = return Nothing

getRoster :: (MonadStream m) => RosterPlugin m -> m Roster
getRoster RosterPlugin {rosterPluginRef} = do
  r <- readIORef rosterPluginRef
  case r of
    Left future -> readMVar future
    Right res -> return res

tryGetRoster :: (MonadStream m) => RosterPlugin m -> m (Maybe Roster)
tryGetRoster RosterPlugin {rosterPluginRef} = toRight <$> readIORef rosterPluginRef

getRosterPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (RosterPlugin m)
getRosterPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (RosterPlugin m)) $ pluginsHooksSet pluginsRef

rosterPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
rosterPlugin pluginsRef = do
  let old = pluginsOldCacheFor pluginsRef (Proxy :: Proxy (RosterPlugin m)) >>= JSON.parseMaybe parseJSON
  firstRoster <- newEmptyMVar
  rosterPluginRef <- newIORef (Left firstRoster)
  rosterPluginSlot <- Slot.new
  let rosterPluginSession = pluginsSession pluginsRef
      plugin :: RosterPlugin m = RosterPlugin {..}
      tryGet = sendFirstRequest rosterPluginSession old $ \ret -> do
        atomicWriteIORef rosterPluginRef (Right ret)
        putMVar firstRoster ret
  void $ forkIO $ tryGet `catch` \(e :: SomeException) -> putMVar firstRoster (throw e)
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  registerCacheGetter pluginsRef plugin
  HL.pushNewOrFailM plugin $ pluginsIQHandlers pluginsRef
