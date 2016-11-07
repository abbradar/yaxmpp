module Network.XMPP.Roster
  ( SubscriptionType(..)

  , RosterAddress
  , rosterAddress
  , showRosterAddress

  , RosterEntry(..)
  , Roster
  , rosterEntries
  , RosterRef
  , getRoster
  , rosterPlugin
  ) where

import Data.Maybe
import Data.Monoid
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Concurrent.Lifted
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Text.InterpolatedString.Perl6 (qq)

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

showRosterAddress :: RosterAddress -> Text
showRosterAddress (RosterAddress {..}) = raddrLocal <> "@" <> raddrDomain


data RosterEntry = RosterEntry { rentryName :: Maybe Text
                               , rentrySubscription :: SubscriptionType
                               , rentryGroups :: Set Text
                               }
                 deriving (Show, Eq)

data Roster = Roster { rosterVersion :: Maybe Text
                     , rosterEntries :: Map RosterAddress RosterEntry
                     }
            deriving (Show, Eq)

newtype RosterRef = RosterRef { unRRef :: MVar Roster }

rosterName :: Text -> Name
rosterName = nsName "jabber:iq:roster"

getRoster :: MonadSession m => RosterRef -> m Roster
getRoster = readMVar . unRRef

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

handleFirstRequest :: MonadSession m => MVar Roster -> Maybe Roster -> ResponseIQHandler m
handleFirstRequest rref _ (Left (err, _)) = putMVar rref $ error [qq|handleFirstRequest: error while requesting roster: $err|]
handleFirstRequest rref (Just old) (Right []) = putMVar rref old
handleFirstRequest rref _ (Right [res])
  | elementName res == rosterName "query" =
    putMVar rref $ case sequence $ map getInitialEntry $ fromElement res $/ curElement of
                     Left e -> error $ T.unpack e
                     Right entries -> Roster { rosterVersion = getAttr "ver" res
                                            , rosterEntries = M.fromList entries
                                            }
handleFirstRequest rref _ _ = putMVar rref $ error "handleFirstRequest: invalid roster response"

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
  
rosterIqHandler :: MonadSession m => XMPPAddress -> MVar Roster -> InRequestIQ -> m (Maybe (Either StanzaError [Element]))
rosterIqHandler myAddr rref iq
  | iriTo iq == Just myAddr && iriType iq == IQSet
  , [req] <- iriChildren iq
  , elementName req == rosterName "query" = fmap Just $ modifyMVar rref $ \roster -> do
      let newEntries = foldM applyUpdate (rosterEntries roster) $ fromElement req $/ curElement
      case newEntries of
        Left err -> return (roster, Left err)
        Right rosterEntries -> do
          let roster' = Roster { rosterVersion = getAttr "ver" req
                               , ..
                               }
          return (roster', Right [])
rosterIqHandler _ _ _ = return Nothing
  
rosterPlugin :: MonadSession m => Maybe Roster -> StanzaSession m -> m (XMPPPlugin m, RosterRef)
rosterPlugin old sess = do
  rref <- newEmptyMVar
  stream <- sessionGetStream $ ssSession sess
  let old' = do
        res <- old
        void $ rosterVersion res
        unless (any (\case RosterVersioning -> True; _ -> False) $ streamFeatures stream) $ fail "rosterPlugin: roster versioning is not supported"
        return res
  let req = OutRequestIQ { oriTo = Nothing
                         , oriIqType = IQGet
                         , oriChildren = [element (rosterName "query") (maybeToList $ fmap ("id", ) $ old' >>= rosterVersion) []]
                         }
  stanzaRequest sess req $ handleFirstRequest rref old'

  let plugin = XMPPPlugin { pluginData = rref
                          , pluginInHandler = \_ _ -> return Nothing
                          , pluginRequestIqHandler = rosterIqHandler $ sessionAddress $ ssSession sess
                          }
  return (plugin, RosterRef rref)
