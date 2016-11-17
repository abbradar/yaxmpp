module Network.XMPP.Presence
  ( ShowState(..)
  , PresenceHandler
  , Presence(..)
  , presencePlugin
  , PresenceEvent(..)
  , presenceUpdate
  ) where

import Data.Int
import Control.Monad
import Text.Read (readMaybe)
import Text.XML
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Logger
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.Default.Class
import Text.InterpolatedString.Perl6 (qq)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Injective
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

type PresenceHandler m = FullJID -> Either [Element] Presence -> m Bool

data PresenceRef m = PresenceRef { presenceHandlers :: [PresenceHandler m]
                                 }

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

emitPresence :: MonadSession m => PresenceRef m -> FullJID -> Either [Element] Presence -> m ()
emitPresence pref addr pres = tryHandlers $ presenceHandlers pref
  where tryHandlers [] = $(logWarn) [qq|Unhandled presence update for $addr: $pres|]
        tryHandlers (handler:handlers) = do
          r <- handler addr pres
          if r then return () else tryHandlers handlers

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
  let presenceExtended = parseExtended elems

  return Presence {..}

parseExtended :: [Element] -> [Element]
parseExtended elems = fromChildren elems $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement

presenceInHandler :: MonadSession m => PresenceRef m -> PluginInHandler m
presenceInHandler pref@(PresenceRef {..}) (InStanza { istFrom = Just addr, istType = InPresence (Right (presenceOp -> Just op)), istChildren }) = Just <$> do
  case fullJidGet addr of
    Nothing -> return $ Just $ jidMalformed "presenceInHandler: Presence should be announced for a full-specified JID"
    Just faddr ->
      case op of
        PresenceSet -> case parsePresence istChildren of
          Right p -> do
            emitPresence pref faddr $ Right p
            return Nothing
          Left e -> return $ Just e
        PresenceUnset -> do
            emitPresence pref faddr $ Left $ parseExtended istChildren
            return Nothing
presenceInHandler _ _ = return Nothing

presencePlugin :: MonadSession m => [PresenceHandler m] -> m (XMPPPlugin m)
presencePlugin presenceHandlers = do
  let pref = PresenceRef {..}
      plugin = def { pluginInHandler = presenceInHandler pref }
  return plugin

data PresenceEvent k = Added k Presence
                     | Updated k Presence
                     | Removed k [Element]
                     deriving (Show, Eq)

presenceUpdate :: Ord k => k -> Either [Element] Presence -> Map k Presence -> Maybe (Map k Presence, PresenceEvent k)
presenceUpdate k (Right v) m
  | otherwise = Just (M.insert k v m, Added k v)
  | M.member k m = Just (M.insert k v m, Updated k v)
presenceUpdate k (Left e) m
  | M.member k m = Just (M.delete k m, Removed k e)
  | otherwise = Nothing
