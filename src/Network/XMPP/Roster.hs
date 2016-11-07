module Network.XMPP.Roster
  ( SubscriptionType(..)

  , RosterAddress
  , raddrLocal
  , raddrDomain
  , rosterAddress
  , showRosterAddress

  , RosterEntry(..)
  , Roster
  , rosterVersion
  , rosterEntries
  , RosterRef
  , subscribeRoster
  , getRoster
  , tryGetRoster
  , insertRoster
  , deleteRoster
  , rosterPlugin
  ) where

import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad
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

import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Data.Injective
import Network.XMPP.XML
import Network.XMPP.Address.Internal
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

data RosterAddress = RosterAddress { raddrLocal :: Text
                                   , raddrDomain :: Text
                                   }
                   deriving (Show, Eq, Ord)

rosterAddress :: XMPPAddress -> Maybe RosterAddress
rosterAddress addr = do
  when (isJust $ xmppResource addr) $ fail "rosterAddress: roster address doesn't include resource"
  raddrLocal <- xmppLocal addr
  return RosterAddress { raddrDomain = xmppDomain addr
                       ,  ..
                       }

rosterXMPPAddress :: RosterAddress -> XMPPAddress
rosterXMPPAddress (RosterAddress {..}) = XMPPAddress { xmppLocal = Just raddrLocal
                                                     , xmppDomain = raddrDomain
                                                     , xmppResource = Nothing
                                                     }

instance FromJSON RosterAddress where
  parseJSON v = do
    addr <- parseJSON v
    case rosterAddress addr of
      Nothing -> fail "RosterAddress"
      Just r -> return r

instance FromJSONKey RosterAddress where
  fromJSONKey = FromJSONKeyTextParser $ \k -> case readXMPPAddress k >>= rosterAddress of
    Nothing -> fail "RosterAddress"
    Just r -> return r

instance ToJSON RosterAddress where
  toJSON = toJSON . rosterXMPPAddress

instance ToJSONKey RosterAddress where
  toJSONKey = JSON.toJSONKeyText (showXMPPAddress . rosterXMPPAddress)

showRosterAddress :: RosterAddress -> Text
showRosterAddress (RosterAddress {..}) = raddrLocal <> "@" <> raddrDomain


data RosterEntry = RosterEntry { rentryName :: Maybe Text
                               , rentrySubscription :: SubscriptionType
                               , rentryGroups :: Set Text
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

data Roster = Roster { rosterVersion :: Maybe Text
                     , rosterEntries :: Map RosterAddress RosterEntry
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

okHandler :: MonadSession m => ResponseIQHandler m
okHandler (Left (err, _)) = fail [qq|okHandler: error while updating a roster: $err|]
okHandler (Right _) = return ()

insertRoster :: MonadSession m => RosterAddress -> RosterEntry -> StanzaSession m -> m ()
insertRoster jid (RosterEntry {..}) sess = stanzaRequest sess request okHandler
  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element "item"
               ([ ("jid", showRosterAddress jid)
                , ("subscription", injTo rentrySubscription)
                ] ++ maybeToList (fmap ("name", ) rentryName)
               ) $ map (\g -> NodeElement $ element "group" [] [NodeContent g]) $ S.toList rentryGroups

deleteRoster :: MonadSession m => RosterAddress -> StanzaSession m -> m ()
deleteRoster jid sess = stanzaRequest sess request okHandler
  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element "item" [("jid", showRosterAddress jid), ("subscription", "remove")] []

getInitialEntry :: Element -> Either Text (RosterAddress, RosterEntry)
getInitialEntry e = do
  unless (elementName e == "item") $ Left "getInitialEntry: invalid item"
  jid <- case getAttr "jid" e >>= readXMPPAddress >>= rosterAddress of
    Nothing -> Left "getInitialEntry: malformed jid"
    Just r -> return r
  rentrySubscription <- case injFrom $ fromMaybe "none" $ getAttr "subscription" e of
    Nothing -> Left "getInitialEntry: invalid subscription type"
    Just r -> return r
  let entry = RosterEntry { rentryName = getAttr "name" e
                          , rentryGroups = S.fromList $ fromElement e $/ XC.element "group" &/ content
                          , ..
                          }
  return (jid, entry)
  
data RosterRef m = RosterRef { rref :: MVar Roster
                             , rrefSignal :: Signal m Roster
                             }

handleFirstRequest :: MonadSession m => RosterRef m -> Maybe Roster -> ResponseIQHandler m
handleFirstRequest (RosterRef {..}) mold resp = do
  let mroster = case (mold, resp) of
        (_, Left (err, _)) -> Left [qq|handleFirstRequest: error while requesting roster: $err|]
        (Just old, Right []) -> Right old
        (_, Right [res]) | elementName res == rosterName "query" ->
                     case sequence $ map getInitialEntry $ fromElement res $/ curElement of
                       Left e -> Left e
                       Right entries -> Right $ Roster { rosterVersion = getAttr "ver" res
                                                      , rosterEntries = M.fromList entries
                                                      }
        _ -> Left "handleFirstRequest: invalid roster response"

  case mroster of
    Left e -> putMVar rref $ error $ T.unpack e
    Right roster' -> do
      putMVar rref roster'
      Signal.emit rrefSignal roster'

applyUpdate :: Map RosterAddress RosterEntry -> Element -> Either StanzaError (Map RosterAddress RosterEntry)
applyUpdate entries e = do
  unless (elementName e == "item") $ Left $ badRequest "applyUpdate: invalid item"
  jid <- case getAttr "jid" e >>= readXMPPAddress >>= rosterAddress of
    Nothing -> Left $ jidMalformed "applyUpdate: invalid jid in roster push"
    Just r -> return r
  let subscr = fromMaybe "none" (getAttr "subscription" e)
  case injFrom subscr of
    Just rentrySubscription -> do
      let entry' = RosterEntry { rentryName = getAttr "name" e
                               , rentryGroups = S.fromList $ fromElement e $/ XC.element "group" &/ content
                               , ..
                               }
      return $ M.insert jid entry' entries
    Nothing | subscr == "remove" -> return $ M.delete jid entries
    _ -> Left $ badRequest "applyUpdate: invalid subscription attribute"

rosterIqHandler :: MonadSession m => RosterRef m -> InRequestIQ -> m (Maybe (Either StanzaError [Element]))
rosterIqHandler (RosterRef {..}) iq
  | iriType iq == IQSet
  , [req] <- iriChildren iq
  , elementName req == rosterName "query" = fmap Just $ do
      mroster <- modifyMVar rref $ \roster -> do
        let newEntries = foldM applyUpdate (rosterEntries roster) $ fromElement req $/ curElement
        case newEntries of
          Left err -> return (roster, Left err)
          Right rosterEntries -> do
            let roster' = Roster { rosterVersion = getAttr "ver" req
                                 , ..
                                 }
            return (roster', Right roster')
      case mroster of
        Left err -> return $ Left err
        Right roster' -> do
          Signal.emit rrefSignal roster'
          return $ Right []
rosterIqHandler _ _ = return Nothing
  
subscribeRoster :: MonadSession m => RosterRef m -> (Roster -> m ()) -> m ()
subscribeRoster (RosterRef {..}) = Signal.subscribe rrefSignal

getRoster :: MonadSession m => RosterRef m -> m Roster
getRoster = readMVar . rref

tryGetRoster :: MonadSession m => RosterRef m -> m (Maybe Roster)
tryGetRoster = tryReadMVar . rref

rosterPlugin :: MonadSession m => Maybe Roster -> StanzaSession m -> m (XMPPPlugin m, RosterRef m)
rosterPlugin old sess = do
  rref <- newEmptyMVar
  rrefSignal <- Signal.empty
  stream <- sessionGetStream $ ssSession sess
  let old' = do
        res <- old
        void $ rosterVersion res
        unless (any (\case RosterVersioning -> True; _ -> False) $ streamFeatures stream) $ fail "rosterPlugin: roster versioning is not supported"
        return res
      req = serverRequest IQGet [element (rosterName "query") (maybeToList $ fmap ("ver", ) $ old' >>= rosterVersion) []]
      rosterRef = RosterRef {..}

  stanzaRequest sess req $ handleFirstRequest rosterRef old'
  let plugin = XMPPPlugin { pluginInHandler = \_ -> return Nothing
                          , pluginRequestIqHandler = rosterIqHandler rosterRef
                          }
  return (plugin, rosterRef)
