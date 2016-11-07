module Network.XMPP.Roster
  ( SubscriptionType(..)

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
                     , rosterEntries :: Map XMPPAddress RosterEntry
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

data RosterRef m = RosterRef { rref :: MVar Roster
                             , rrefSignal :: Signal m Roster
                             , rrefSession :: StanzaSession m
                             }

insertRoster :: MonadSession m => XMPPAddress -> RosterEntry -> RosterRef m -> m ()
insertRoster jid (RosterEntry {..}) (RosterRef {..}) = do
  void $ readMVar rref
  stanzaRequest rrefSession request okHandler

  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element "item"
               ([ ("jid", showXMPPAddress jid)
                , ("subscription", injTo rentrySubscription)
                ] ++ maybeToList (fmap ("name", ) rentryName)
               ) $ map (\g -> NodeElement $ element "group" [] [NodeContent g]) $ S.toList rentryGroups

deleteRoster :: MonadSession m => XMPPAddress -> RosterRef m -> m ()
deleteRoster jid (RosterRef {..}) = do
  void $ readMVar rref
  stanzaRequest rrefSession request okHandler

  where request = serverRequest IQSet [element (rosterName "query") [] [NodeElement item]]
        item = element "item" [("jid", showXMPPAddress jid), ("subscription", "remove")] []

getInitialEntry :: Element -> Either Text (XMPPAddress, RosterEntry)
getInitialEntry e = do
  unless (elementName e == rosterName "item") $ Left [qq|getInitialEntry: invalid roster item $e|]
  jid <- case getAttr "jid" e >>= readXMPPAddress of
    Nothing -> Left "getInitialEntry: malformed jid"
    Just r -> return r
  rentrySubscription <- case injFrom $ fromMaybe "none" $ getAttr "subscription" e of
    Nothing -> Left "getInitialEntry: invalid subscription type"
    Just r -> return r
  let entry = RosterEntry { rentryName = getAttr "name" e
                          , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
                          , ..
                          }
  return (jid, entry)

handleFirstRequest :: MonadSession m => RosterRef m -> Maybe Roster -> ResponseIQHandler m
handleFirstRequest (RosterRef {..}) mold resp = do
  let roster' = case (mold, resp) of
        (_, Left (err, _)) -> error [qq|handleFirstRequest: error while requesting roster: $err|]
        (Just old, Right []) | isJust $ rosterVersion old  -> old
        (_, Right [res]) | elementName res == rosterName "query" ->
                     case sequence $ map getInitialEntry $ fromElement res $/ curAnyElement of
                       Left e -> error $ T.unpack e
                       Right entries -> Roster { rosterVersion = getAttr "ver" res
                                              , rosterEntries = M.fromList entries
                                              }
        _ -> error "handleFirstRequest: invalid roster response"

  putMVar rref roster'
  Signal.emit rrefSignal roster'

applyUpdate :: Map XMPPAddress RosterEntry -> Element -> Either StanzaError (Map XMPPAddress RosterEntry)
applyUpdate entries e = do
  unless (elementName e == rosterName "item") $ Left $ badRequest [qq|applyUpdate: invalid roster item $e|]
  jid <- case getAttr "jid" e >>= readXMPPAddress of
    Nothing -> Left $ jidMalformed "applyUpdate: invalid jid in roster push"
    Just r -> return r
  let subscr = fromMaybe "none" (getAttr "subscription" e)
  case injFrom subscr of
    Just rentrySubscription -> do
      let entry' = RosterEntry { rentryName = getAttr "name" e
                               , rentryGroups = S.fromList $ fromElement e $/ XC.element (rosterName "group") &/ content
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
        let newEntries = foldM applyUpdate (rosterEntries roster) $ fromElement req $/ curAnyElement
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
rosterPlugin old rrefSession = do
  rref <- newEmptyMVar
  rrefSignal <- Signal.empty
  let rosterRef = RosterRef {..}

  stream <- sessionGetStream $ ssSession rrefSession
  let ver =
        if any (\case RosterVersioning -> True; _ -> False) $ streamFeatures stream
        then Just $ fromMaybe "" $ old >>= rosterVersion
        else Nothing
      req = serverRequest IQGet [element (rosterName "query") (maybeToList $ fmap ("ver", ) ver) []]

  stanzaRequest rrefSession req $ handleFirstRequest rosterRef old
  let plugin = XMPPPlugin { pluginInHandler = \_ -> return Nothing
                          , pluginRequestIqHandler = rosterIqHandler rosterRef
                          }
  return (plugin, rosterRef)
