module Network.XMPP.Presence
  ( ShowState(..)
  , Presence(..)
  , PresenceRef
  , presenceSubscribe
  , presenceSend
  , presencePlugin
  ) where

import Data.Maybe
import Data.Int
import Control.Monad
import Text.Read (readMaybe)
import Control.Concurrent.MVar.Lifted
import Text.XML
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Data.Injective
import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Network.XMPP.XML
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Address

data ShowState = ShowAway
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

data Presence = Presence { presenceShow :: Maybe ShowState
                         , presenceStatus :: Map XMLLang Text
                         , presencePriority :: Int8
                         , presenceChildren :: [Element]
                         }
              deriving (Show, Eq)

type PresenceMap = Map (Text, Text) (Map Text Presence)

data PresenceRef m = PresenceRef { presenceRef :: MVar PresenceMap
                                 , presenceSignal :: Signal m (XMPPAddress, Maybe Presence)
                                 , presenceSession :: StanzaSession m
                                 }

splitAddress :: XMPPAddress -> Maybe ((Text, Text), Text)
splitAddress addr = do
  local <- xmppLocal addr
  resource <- xmppResource addr
  return ((local, xmppDomain addr), resource)

data PresenceOp = PresenceSet
                | PresenceUnset
                deriving (Show, Eq)

presenceOp :: Maybe PresenceType -> Maybe PresenceOp
presenceOp Nothing = Just PresenceSet
presenceOp (Just PresenceUnavailable) = Just PresenceUnset
presenceOp _ = Nothing

readIntMaybe :: forall a. (Bounded a, Integral a) => String -> Maybe a
readIntMaybe str = do
  (i :: Integer) <- readMaybe str
  when (i < fromIntegral (minBound :: a)) $ fail "readIntMaybe: too small"
  when (i > fromIntegral (maxBound :: a)) $ fail "readIntMaybe: too big"
  return $ fromIntegral i

parsePresence :: [Element] -> Either StanzaError Presence
parsePresence elems = do
  let cur = fromChildren elems
  let readStatus e = (getAttr (xmlName "lang") e, mconcat $ fromElement e $/ content)
  
  presenceShow <- case cur $/ XC.element "show" &/ content of
    [val] -> case injFrom val of
      Just sh -> return $ Just sh
      Nothing -> Left $ badRequest "parsePresence: invalid show"
    [] -> return Nothing
    _ -> Left $ badRequest "parsePresence: multiple show values"
  let presenceStatus = M.fromList $ map readStatus $ cur $/ XC.element "status" &| curElement
  presencePriority <- case cur $/ XC.element "priority" &/ content of
    [val] -> case readIntMaybe $ T.unpack val of
      Nothing -> Left $ badRequest "parsePresence: invalid priority value"
      Just r -> return r
    [] -> return 0
    _ -> Left $ badRequest "parsePresence: multiple priority values"
  let presenceChildren = cur $/ checkName (isJust . nameNamespace) &| curElement

  return Presence {..}

presenceInHandler :: MonadSession m => PresenceRef m -> InStanza -> m (Maybe (Maybe StanzaError))
presenceInHandler (PresenceRef {..}) (InStanza { istFrom = Just addr, istType = InPresence (Right (presenceOp -> Just op)), istChildren }) = Just <$> do
  case splitAddress addr of
    Nothing -> return $ Just $ jidMalformed "presenceInHandler: Presence should be announced for a full-specified JID"
    Just (bare, res) -> do
      ret <- modifyMVar presenceRef $ \pres -> case op of
        PresenceSet -> case parsePresence istChildren of
          Right p -> do
            let pres' = M.insertWith M.union bare (M.singleton res p) pres
            return (pres', Right (addr, Just p))
          Left e -> return (pres, Left e)
        PresenceUnset -> do
          let removeRes m = if M.null m' then Nothing else Just m'
                where m' = M.delete res m
              pres' = M.update removeRes bare pres
          return (pres', Right (addr, Nothing))
      case ret of
        Left err -> return $ Just err
        Right r -> do
          Signal.emit presenceSignal r
          return Nothing
presenceInHandler _ _ = return Nothing

presenceSubscribe :: MonadSession m => PresenceRef m -> ((XMPPAddress, Maybe Presence) -> m ()) -> m ()
presenceSubscribe (PresenceRef {..}) = Signal.subscribe presenceSignal

presenceSend :: MonadSession m => PresenceRef m -> Presence -> m ()
presenceSend (PresenceRef {..}) (Presence {..}) =
  void $ stanzaSend presenceSession OutStanza { ostTo = Nothing
                                              , ostType = OutPresence Nothing
                                              , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceChildren
                                              }

  where priority = element "priority" [] [NodeContent $ T.pack $ show presencePriority]
        mShow = fmap (\s -> element "show" [] [NodeContent $ injTo s]) presenceShow
        statuses = map (\(lang, s) -> element "status" (maybeToList $ fmap (xmlName "lang", ) lang) [NodeContent s]) $ M.toList presenceStatus

presencePlugin :: MonadSession m => StanzaSession m -> m (XMPPPlugin m, PresenceRef m)
presencePlugin presenceSession = do
  presenceRef <- newMVar M.empty
  presenceSignal <- Signal.empty
  let pref = PresenceRef {..}
      plugin = XMPPPlugin { pluginInHandler = presenceInHandler pref
                          , pluginRequestIqHandler = \_ -> return Nothing
                          }
  return (plugin, pref)
