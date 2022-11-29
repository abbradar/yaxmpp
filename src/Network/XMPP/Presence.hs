{-# LANGUAGE Strict #-}

module Network.XMPP.Presence
  ( ShowState(..)
  , PresenceHandler
  , Presence(..)
  , defaultPresence
  , PresenceRef
  , presenceHandlers
  , presencePlugin
  , PresenceEvent(..)
  , presenceUpdate
  , presenceStanza
  ) where

import Data.Int
import Data.Maybe
import Control.Monad
import Text.XML
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Logger
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.String.Interpolate (i)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import TextShow (showt)

import Control.HandlerList (HandlerList, HandlerListRef)
import qualified Control.HandlerList as HandlerList
import Data.Injective
import Network.XMPP.Utils
import Network.XMPP.XML
import Network.XMPP.Stream
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

defaultPresence :: Presence
defaultPresence = Presence { presenceShow = Nothing
                           , presenceStatus = Nothing
                           , presencePriority = 0
                           , presenceExtended = []
                           }

type PresenceHandler m = (FullJID, Either [Element] Presence) -> m (Maybe ())

newtype PresenceRef m = PresenceRef { presenceHandlersI :: HandlerList m (FullJID, Either [Element] Presence) ()
                                    }

data PresenceOp = PresenceSet
                | PresenceUnset
                deriving (Show, Eq)

presenceOp :: Maybe PresenceType -> Maybe PresenceOp
presenceOp Nothing = Just PresenceSet
presenceOp (Just PresenceUnavailable) = Just PresenceUnset
presenceOp _ = Nothing

presenceHandlers :: PresenceRef m -> HandlerListRef m (FullJID, Either [Element] Presence) ()
presenceHandlers pref = HandlerList.ref $ presenceHandlersI pref

emitPresence :: MonadStream m => PresenceRef m -> FullJID -> Either [Element] Presence -> m ()
emitPresence (PresenceRef {..}) addr pres = do
  mr <- HandlerList.call presenceHandlersI (addr, pres)
  case mr of
    Nothing -> $(logWarn) [i|Unhandled presence update for #{addr}: #{pres}|]
    Just () -> return ()

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

presenceInHandler :: MonadStream m => PresenceRef m -> PluginInHandler m
presenceInHandler pref (InStanza { istType = InPresence (Right (presenceOp -> Just op)), istFrom = Just (fullJidGet -> Just faddr), istChildren }) = Just <$> do
  case op of
    PresenceSet -> case parsePresence istChildren of
      Right p -> do
        emitPresence pref faddr $ Right p
        return InSilent
      Left e -> return $ InError e
    PresenceUnset -> do
        emitPresence pref faddr $ Left $ parseExtended istChildren
        return InSilent
presenceInHandler _ _ = return Nothing

presencePlugin :: MonadStream m => XMPPPluginsRef m -> m (PresenceRef m)
presencePlugin pluginsRef = do
  presenceHandlersI <- HandlerList.new
  let pref = PresenceRef {..}
  void $ HandlerList.add (pluginInHandlers pluginsRef) (presenceInHandler pref)
  return pref

data PresenceEvent k = Added k Presence
                     | Updated k Presence
                     | Removed k [Element]
                     deriving (Show, Eq)

presenceUpdate :: Ord k => k -> Either [Element] Presence -> Map k Presence -> Maybe (Map k Presence, PresenceEvent k)
presenceUpdate k (Right v) m
  | M.member k m = Just (M.insert k v m, Updated k v)
  | otherwise = Just (M.insert k v m, Added k v)
presenceUpdate k (Left e) m
  | M.member k m = Just (M.delete k m, Removed k e)
  | otherwise = Nothing

presenceStanza :: Maybe Presence -> OutStanza
presenceStanza (Just (Presence {..})) =
  OutStanza { ostTo = Nothing
            , ostType = OutPresence Nothing
            , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceExtended
            }
  where priority = element (jcName "priority") [] [NodeContent $ showt presencePriority]
        mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
        statuses = maybe [] (localizedElements $ jcName "status") presenceStatus
presenceStanza Nothing =
  OutStanza { ostTo = Nothing
            , ostType = OutPresence (Just PresenceUnavailable)
            , ostChildren = []
            }
