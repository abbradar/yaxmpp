{-# LANGUAGE Strict #-}

module Network.XMPP.Roster
  ( SubscriptionType(..)
  , RosterEntry(..)
  , RosterEntries
  , Roster
  , rosterVersion
  , rosterEntries
  , RosterEvent(..)
  , RosterRef
  , getRoster
  , tryGetRoster
  , insertRoster
  , deleteRoster
  , rosterSlot
  , rosterPlugin
  ) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Exception (SomeException, throw)
import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import UnliftIO.IORef
import UnliftIO.MVar
import UnliftIO.Concurrent
import UnliftIO.Exception (catch)
import Text.XML
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Aeson.Types as JSON
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.String.Interpolate (i)

import Control.Slot (Slot, SlotRef)
import qualified Control.Slot as Slot
import qualified Control.HandlerList as HandlerList
import Data.Injective
import Network.XMPP.XML
import Network.XMPP.Utils
import Network.XMPP.Address
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin

data SubscriptionType = SubNone
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

data RosterEntry = RosterEntry { rentryName :: Maybe RosterName
                               , rentrySubscription :: SubscriptionType
                               , rentrySubscriptionAsked :: Bool
                               , rentryGroups :: Set RosterGroup
                               }
                 deriving (Show, Eq, Generic)

prefixedField :: String -> String -> String
prefixedField prefix = JSON.camelTo2 '_' . fromJust . stripPrefix prefix

rosterEntryOptions :: JSON.Options
rosterEntryOptions = JSON.defaultOptions { JSON.fieldLabelModifier = prefixedField "rentry" }

instance FromJSON RosterEntry where
  parseJSON = genericParseJSON rosterEntryOptions

instance ToJSON RosterEntry where
  toJSON = genericToJSON rosterEntryOptions
  toEncoding = genericToEncoding rosterEntryOptions

type RosterEntries = Map XMPPAddress RosterEntry

data Roster = Roster { rosterVersion :: Maybe Text
                     , rosterEntries :: RosterEntries
                     }
            deriving (Show, Eq, Generic)

rosterOptions :: JSON.Options
rosterOptions = JSON.defaultOptions { JSON.fieldLabelModifier = prefixedField "roster" }

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

okHandler :: MonadStream m => IQResponseHandler m
okHandler (Left err) = $(logError) [i|okHandler: error while updating a roster: #{err}|]
okHandler (Right _) = return ()

data RosterEvent = RosterInsert XMPPAddress RosterEntry
                 | RosterDelete XMPPAddress
                 deriving (Show, Eq)

data RosterRef m = RosterRef { rref :: IORef (Either (MVar Roster) Roster)
                             , rrefSlot :: Slot m (RosterEntries, RosterEvent)
                             , rrefSession :: StanzaSession m
                             }

insertRoster :: MonadStream m => RosterRef m -> XMPPAddress -> Maybe RosterName -> Set RosterGroup -> m ()
insertRoster rr@(RosterRef {..}) jid name groups = do
  void $ getRoster rr
  stanzaRequest rrefSession request okHandler

  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element (rosterName "item")
               (("jid", addressToText jid) : maybeToList (fmap ("name", ) name))
               $ map (\g -> NodeElement $ element (rosterName "group") [] [NodeContent g]) $ S.toList groups

deleteRoster :: MonadStream m => RosterRef m -> XMPPAddress -> m ()
deleteRoster rr@(RosterRef {..}) jid = do
  void $ getRoster rr
  stanzaRequest rrefSession request okHandler

  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
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
  let entry = RosterEntry { rentryName = getAttr "name" e
                          , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
                          , ..
                          }
  return (jid, entry)

sendFirstRequest :: MonadStream m => StanzaSession m -> Maybe Roster -> m Roster
sendFirstRequest session mold = do
  stream <- sessionGetStream $ ssSession session
  let ver =
        if any isRosterVer $ streamFeatures stream
        then Just $ fromMaybe "" $ mold >>= rosterVersion
        else Nothing
      req = serverRequest IQGet [element (rosterName "query") (maybeToList $ fmap ("ver", ) ver) []]

  resp <- stanzaSyncRequest session req

  case (mold, resp) of
        (_, Left err) -> fail [i|handleFirstRequest: error while requesting roster: #{err}|]
        (Just old, Right []) | isJust $ rosterVersion old  -> return old
        (_, Right [res]) | elementName res == rosterName "query" ->
                     case mapM parseInitial $ fromElement res $/ curAnyElement of
                       Left e -> fail $ T.unpack e
                       Right entries -> return $ Roster { rosterVersion = getAttr "ver" res
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
      let entry' = RosterEntry { rentryName = getAttr "name" e
                               , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
                               , ..
                               }
      return $ RosterInsert jid entry'
    Nothing | subscr == "remove" -> return $ RosterDelete jid
    _ -> Left $ badRequest "getRosterEvent: invalid subscription attribute"

applyRosterEvent :: RosterEntries -> RosterEvent -> RosterEntries
applyRosterEvent roster (RosterInsert jid entry) = M.insert jid entry roster
applyRosterEvent roster (RosterDelete jid) = M.delete jid roster

rosterIQHandler :: MonadStream m => RosterRef m -> PluginIQHandler m
rosterIQHandler (RosterRef {..}) (InRequestIQ { iriType = IQSet, iriFrom, iriChildren = [req] })
  | elementName req == rosterName "query" = Just <$> do
      if not (fromServerOrMyself iriFrom rrefSession) then
        return $ IQError $ serviceUnavailable "Roster push from arbitrary entity is forbidden"
      else do
        case mapM getRosterEvent $ fromElement req $/ curAnyElement of
          Left err -> return $ IQError err
          Right events -> do
            result <- atomicModifyIORef' rref $ \case
              pending@(Left _) -> (pending, Nothing)
              Right roster ->
                let rosters = tail $ scanl applyRosterEvent (rosterEntries roster) events
                    roster' = Roster { rosterVersion = getAttr "ver" req
                                     , rosterEntries = last rosters
                                     }
                in (Right roster', Just $ zip rosters events)
            case result of
              Nothing -> return $ IQError $ badRequest "Roster has not been received yet"
              Just revents -> do
                mapM_ (Slot.call rrefSlot) revents
                return $ IQResult []
rosterIQHandler _ _ = return Nothing

getRoster :: MonadStream m => RosterRef m -> m Roster
getRoster (RosterRef {..}) = do
  r <- readIORef rref
  case r of
    Left future -> readMVar future
    Right res -> return res

tryGetRoster :: MonadStream m => RosterRef m -> m (Maybe Roster)
tryGetRoster (RosterRef {..}) = toRight <$> readIORef rref

rosterSlot :: MonadStream m => RosterRef m -> SlotRef m (RosterEntries, RosterEvent)
rosterSlot (RosterRef {..}) = Slot.ref rrefSlot

rosterPlugin :: MonadStream m => XMPPPluginsRef m -> Maybe Roster -> m (RosterRef m)
rosterPlugin pluginsRef old = do
  firstRoster <- newEmptyMVar
  rref <- newIORef (Left firstRoster)
  let rrefSession = pluginsSession pluginsRef
      tryGet = do
        ret <- sendFirstRequest rrefSession old
        atomicWriteIORef rref (Right ret)
        putMVar firstRoster ret
  void $ forkIO $ tryGet `catch` \(e :: SomeException) -> putMVar firstRoster (throw e)
  rrefSlot <- Slot.new
  let rosterRef = RosterRef {..}
  void $ HandlerList.add (pluginIQHandlers pluginsRef) (rosterIQHandler rosterRef)
  return rosterRef
