module Network.XMPP.Roster
  ( SubscriptionType(..)
  , RosterEntry(..)
  , RosterEntries
  , Roster
  , rosterVersion
  , rosterEntries
  , RosterRef
  , rosterGet
  , rosterTryGet
  , insertRoster
  , deleteRoster
  , rosterSetHandler
  , rosterPlugin
  ) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Concurrent.MVar.Lifted
import Text.XML
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Aeson.Types as JSON
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Text.InterpolatedString.Perl6 (qq)

import Control.Handler (Handler)
import qualified Control.Handler as Handler
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

rosterName :: Text -> Name
rosterName = nsName "jabber:iq:roster"

okHandler :: MonadStream m => ResponseIQHandler m
okHandler (Left (err, _)) = $(logError) [qq|okHandler: error while updating a roster: $err|]
okHandler (Right _) = return ()

data RosterEvent = RosterInsert XMPPAddress RosterEntry
                 | RosterDelete XMPPAddress
                 deriving (Show, Eq)

data RosterRef m = RosterRef { rref :: MVar Roster
                             , rrefHandler :: Handler m (RosterEntries, RosterEvent)
                             , rrefSession :: StanzaSession m
                             }

insertRoster :: MonadStream m => RosterRef m -> XMPPAddress -> Maybe RosterName -> Set RosterGroup -> m ()
insertRoster (RosterRef {..}) jid name groups = do
  void $ readMVar rref
  stanzaRequest rrefSession request okHandler

  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element (rosterName "item")
               ([ ("jid", addressToText jid)
                ] ++ maybeToList (fmap ("name", ) name)
               ) $ map (\g -> NodeElement $ element (rosterName "group") [] [NodeContent g]) $ S.toList groups

deleteRoster :: MonadStream m => RosterRef m -> XMPPAddress -> m ()
deleteRoster (RosterRef {..}) jid = do
  void $ readMVar rref
  stanzaRequest rrefSession request okHandler

  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element (rosterName "item") [("jid", addressToText jid), ("subscription", "remove")] []

parseInitial :: Element -> Either Text (XMPPAddress, RosterEntry)
parseInitial e = do
  unless (elementName e == rosterName "item") $ Left [qq|parseInitial: invalid roster item $e|]
  jid <- case getAttr "jid" e >>= (toRight . xmppAddress) of
    Nothing -> Left "parseInitial: malformed jid"
    Just r -> return r
  rentrySubscription <- case injFrom $ fromMaybe "none" $ getAttr "subscription" e of
    Nothing -> Left "parseInitial: invalid subscription type"
    Just r -> return r
  let entry = RosterEntry { rentryName = getAttr "name" e
                          , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
                          , ..
                          }
  return (jid, entry)

handleFirstRequest :: MonadStream m => RosterRef m -> Maybe Roster -> ResponseIQHandler m
handleFirstRequest (RosterRef {..}) mold resp = do
  let roster' = case (mold, resp) of
        (_, Left (err, _)) -> error [qq|handleFirstRequest: error while requesting roster: $err|]
        (Just old, Right []) | isJust $ rosterVersion old  -> old
        (_, Right [res]) | elementName res == rosterName "query" ->
                     case sequence $ map parseInitial $ fromElement res $/ curAnyElement of
                       Left e -> error $ T.unpack e
                       Right entries -> Roster { rosterVersion = getAttr "ver" res
                                              , rosterEntries = M.fromList entries
                                              }
        _ -> error "handleFirstRequest: invalid roster response"

  putMVar rref roster'

getRosterEvent :: Element -> Either StanzaError RosterEvent
getRosterEvent e = do
  unless (elementName e == rosterName "item") $ Left $ badRequest [qq|getRosterEvent: invalid roster item $e|]
  jid <- case getAttr "jid" e >>= (toRight . xmppAddress) of
    Nothing -> Left $ jidMalformed "getRosterEvent: invalid jid in roster push"
    Just r -> return r
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

rosterIqHandler :: MonadStream m => RosterRef m -> PluginRequestIQHandler m
rosterIqHandler (RosterRef {..}) (InRequestIQ { iriType = IQSet, iriChildren = [req] })
  | elementName req == rosterName "query" = Just <$> do
      mroster <- modifyMVar rref $ \roster -> do
        let newEntries = mapM getRosterEvent $ fromElement req $/ curAnyElement
        case newEntries of
          Left err -> return (roster, Left err)
          Right events -> do
            let rosters = tail $ scanl applyRosterEvent (rosterEntries roster) events
            let roster' = Roster { rosterVersion = getAttr "ver" req
                                 , rosterEntries = last rosters
                                 }
            return (roster', Right $ zip rosters events)
      case mroster of
        Left err -> return $ Left err
        Right revents -> do
          mapM_ (Handler.call rrefHandler) revents
          return $ Right []
rosterIqHandler _ _ = return Nothing

rosterGet :: MonadStream m => RosterRef m -> m Roster
rosterGet = readMVar . rref

rosterTryGet :: MonadStream m => RosterRef m -> m (Maybe Roster)
rosterTryGet = tryReadMVar . rref

rosterSetHandler :: MonadStream m => RosterRef m -> ((RosterEntries, RosterEvent) -> m ()) -> m ()
rosterSetHandler (RosterRef {..}) = Handler.set rrefHandler

rosterPlugin :: MonadStream m => StanzaSession m -> Maybe Roster -> m (XMPPPlugin m, RosterRef m)
rosterPlugin rrefSession old = do
  rref <- newEmptyMVar
  rrefHandler <- Handler.new
  let rosterRef = RosterRef {..}

  stream <- sessionGetStream $ ssSession rrefSession
  let ver =
        if any (\case RosterVersioning -> True; _ -> False) $ streamFeatures stream
        then Just $ fromMaybe "" $ old >>= rosterVersion
        else Nothing
      req = serverRequest IQGet [element (rosterName "query") (maybeToList $ fmap ("ver", ) ver) []]

  stanzaRequest rrefSession req $ handleFirstRequest rosterRef old
  let plugin = def { pluginRequestIqHandler = rosterIqHandler rosterRef }
  return (plugin, rosterRef)
