{-# LANGUAGE Strict #-}

module Network.XMPP.Roster (
  SubscriptionType (..),
  RosterEntry (..),
  RosterEntries,
  Roster,
  rosterVersion,
  rosterEntries,
  RosterEvent (..),
  getRoster,
  tryGetRoster,
  insertRoster,
  deleteRoster,
  rosterSlot,
  rosterPlugin,
) where

import Control.Exception (SomeException, throw)
import Control.Monad
import Control.Monad.Logger
import Data.Aeson
import qualified Data.Aeson.Types as JSON
import Data.List
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

prefixedField :: String -> String -> String
prefixedField prefix = JSON.camelTo2 '_' . fromJust . stripPrefix prefix

rosterEntryOptions :: JSON.Options
rosterEntryOptions = JSON.defaultOptions {JSON.fieldLabelModifier = prefixedField "rentry"}

instance FromJSON RosterEntry where
  parseJSON = genericParseJSON rosterEntryOptions

instance ToJSON RosterEntry where
  toJSON = genericToJSON rosterEntryOptions
  toEncoding = genericToEncoding rosterEntryOptions

type RosterEntries = Map XMPPAddress RosterEntry

data Roster = Roster
  { rosterVersion :: Maybe Text
  , rosterEntries :: RosterEntries
  }
  deriving (Show, Eq, Generic)

rosterOptions :: JSON.Options
rosterOptions = JSON.defaultOptions {JSON.fieldLabelModifier = prefixedField "roster"}

instance FromJSON Roster where
  parseJSON = genericParseJSON rosterOptions

instance ToJSON Roster where
  toJSON = genericToJSON rosterOptions
  toEncoding = genericToEncoding rosterOptions

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

data RosterState m = RosterState
  { rosterRef :: IORef (Either (MVar Roster) Roster)
  , rosterSession :: StanzaSession m
  }

insertRoster :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe RosterName -> Set RosterGroup -> m ()
insertRoster pluginsRef jid name groups = do
  void $ getRoster pluginsRef
  let session = pluginsSession pluginsRef
  stanzaRequest session request okHandler
 where
  request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
  item =
    element
      (rosterName "item")
      (("jid", addressToText jid) : maybeToList (fmap ("name",) name))
      $ map (\g -> NodeElement $ element (rosterName "group") [] [NodeContent g])
      $ S.toList groups

deleteRoster :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> m ()
deleteRoster pluginsRef jid = do
  void $ getRoster pluginsRef
  let session = pluginsSession pluginsRef
  stanzaRequest session request okHandler
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

sendFirstRequest :: (MonadStream m) => StanzaSession m -> Maybe Roster -> m Roster
sendFirstRequest session mold = do
  let ver =
        if any isRosterVer $ sessionStreamFeatures $ ssSession session
          then Just $ fromMaybe "" $ mold >>= rosterVersion
          else Nothing
      req = serverRequest IQGet [element (rosterName "query") (maybeToList $ fmap ("ver",) ver) []]

  resp <- stanzaSyncRequest session req

  case (mold, resp) of
    (_, Left err) -> fail [i|handleFirstRequest: error while requesting roster: #{err}|]
    (Just old, Right []) | isJust $ rosterVersion old -> return old
    (_, Right [res]) | elementName res == rosterName "query" ->
      case mapM parseInitial $ fromElement res $/ curAnyElement of
        Left e -> fail $ T.unpack e
        Right entries ->
          return $
            Roster
              { rosterVersion = getAttr "ver" res
              , rosterEntries = M.fromList entries
              }
    _ -> fail "handleFirstRequest: invalid roster response"

getRosterEvent :: Element -> Either StanzaError RosterEvent
getRosterEvent e = do
  unless (elementName e == rosterName "item") $ Left $ badRequest [i|getRosterEvent: invalid roster item #{e}|]
  jid <- case getAttr "jid" e >>= (toRight . xmppAddress) of
    Nothing -> Left $ jidMalformed "getRosterEvent: invalid jid in roster push"
    Just r -> return r
  rentrySubscriptionAsked <- case getAttr "ask" e of
    Nothing -> return False
    Just "subscribe" -> return True
    _ -> Left $ badRequest "getRosterEvent: invalid ask type"
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
    _ -> Left $ badRequest "getRosterEvent: invalid subscription attribute"

applyRosterEvent :: RosterEntries -> RosterEvent -> RosterEntries
applyRosterEvent roster (RosterInsert jid entry) = M.insert jid entry roster
applyRosterEvent roster (RosterDelete jid) = M.delete jid roster

data RosterPlugin m = RosterPlugin
  { rosterPluginSlot :: RosterSlot m
  , rosterPluginState :: RosterState m
  }

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (RosterPlugin m) where
  tryHandle (RosterPlugin {..}) (InRequestIQ {iriType = IQSet, iriFrom, iriChildren = [req]})
    | elementName req == rosterName "query" =
        let RosterState {..} = rosterPluginState
         in Just <$> do
              if not (fromServerOrMyself iriFrom rosterSession)
                then
                  return $ IQError $ serviceUnavailable "Roster push from arbitrary entity is forbidden"
                else do
                  case mapM getRosterEvent $ fromElement req $/ curAnyElement of
                    Left err -> return $ IQError err
                    Right events -> do
                      result <- atomicModifyIORef' rosterRef $ \case
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

getRoster :: forall m. (MonadStream m) => XMPPPluginsRef m -> m Roster
getRoster pluginsRef = do
  RosterState {..} <- RegRef.lookupOrFailM (Proxy :: Proxy (RosterState m)) $ pluginsHooksSet pluginsRef
  r <- readIORef rosterRef
  case r of
    Left future -> readMVar future
    Right res -> return res

tryGetRoster :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (Maybe Roster)
tryGetRoster pluginsRef = do
  RosterState {..} <- RegRef.lookupOrFailM (Proxy :: Proxy (RosterState m)) $ pluginsHooksSet pluginsRef
  toRight <$> readIORef rosterRef

rosterSlot :: (MonadStream m) => XMPPPluginsRef m -> m (RosterSlot m)
rosterSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

rosterPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> Maybe Roster -> m ()
rosterPlugin pluginsRef old = do
  firstRoster <- newEmptyMVar
  rosterRef <- newIORef (Left firstRoster)
  let rosterSession = pluginsSession pluginsRef
      state = RosterState {..}
      tryGet = do
        ret <- sendFirstRequest rosterSession old
        atomicWriteIORef rosterRef (Right ret)
        putMVar firstRoster ret
  void $ forkIO $ tryGet `catch` \(e :: SomeException) -> putMVar firstRoster (throw e)
  rosterPluginSlot <- Slot.new
  let plugin :: RosterPlugin m = RosterPlugin {rosterPluginState = state, ..}
  RegRef.insertNewOrFailM rosterPluginSlot $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM state $ pluginsHooksSet pluginsRef
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM plugin iqHandlers
