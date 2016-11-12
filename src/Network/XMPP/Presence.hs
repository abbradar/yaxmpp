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
import Data.IORef.Lifted
import Text.XML
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.Default.Class

import Data.Injective
import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Network.XMPP.XML
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Address
import Network.XMPP.Language

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
                         , presenceStatus :: Maybe LocalizedText
                         , presencePriority :: Int8
                         , presenceExtended :: [Element]
                         }
              deriving (Show, Eq)

instance Default Presence where
  def = Presence { presenceShow = Nothing
                 , presenceStatus = Nothing
                 , presencePriority = 0
                 , presenceExtended = []
                 }

type PresenceMap = Map (Text, Text) (Map Text Presence)

data PresenceRef m = PresenceRef { presenceRef :: IORef PresenceMap
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
  let presenceExtended = cur $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement

  return Presence {..}

presenceInHandler :: MonadSession m => PresenceRef m -> InStanza -> m (Maybe (Maybe StanzaError))
presenceInHandler (PresenceRef {..}) (InStanza { istFrom = Just addr, istType = InPresence (Right (presenceOp -> Just op)), istChildren }) = Just <$> do
  case splitAddress addr of
    Nothing -> return $ Just $ jidMalformed "presenceInHandler: Presence should be announced for a full-specified JID"
    Just (bare, res) -> do
      pres <- readIORef presenceRef
      ret <- case op of
        PresenceSet -> case parsePresence istChildren of
          Right p -> do
            let pres' = M.insertWith M.union bare (M.singleton res p) pres
            return $ Right (pres', (addr, Just p))
          Left e -> return $ Left e
        PresenceUnset -> do
          let removeRes m = if M.null m' then Nothing else Just m'
                where m' = M.delete res m
              pres' = M.update removeRes bare pres
          return $ Right (pres', (addr, Nothing))
      case ret of
        Left err -> return $ Just err
        Right (pres', r) -> do
          writeIORef presenceRef pres'
          Signal.emit presenceSignal r
          return Nothing
presenceInHandler _ _ = return Nothing

presenceSubscribe :: MonadSession m => PresenceRef m -> ((XMPPAddress, Maybe Presence) -> m ()) -> m ()
presenceSubscribe (PresenceRef {..}) = Signal.subscribe presenceSignal

presenceSend :: MonadSession m => PresenceRef m -> Presence -> m ()
presenceSend (PresenceRef {..}) (Presence {..}) =
  void $ stanzaSend presenceSession OutStanza { ostTo = Nothing
                                              , ostType = OutPresence Nothing
                                              , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceExtended
                                              }

  where priority = element (jcName "priority") [] [NodeContent $ T.pack $ show presencePriority]
        mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
        statuses = maybe [] (localizedElements $ jcName "status") presenceStatus

presencePlugin :: MonadSession m => StanzaSession m -> m (XMPPPlugin m, PresenceRef m)
presencePlugin presenceSession = do
  presenceRef <- newIORef M.empty
  presenceSignal <- Signal.empty
  let pref = PresenceRef {..}
      plugin = def { pluginInHandler = presenceInHandler pref }
  return (plugin, pref)
