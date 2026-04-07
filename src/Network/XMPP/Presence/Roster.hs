{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Roster (
  RosterPresenceMap,
  RosterPresenceEvent (..),
  getRosterPresence,
  rpresenceSlot,
  rpresencePlugin,
)
where

import Control.Monad
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Roster
import Network.XMPP.Stream
import Text.XML
import UnliftIO.IORef

import qualified Control.HandlerList as HandlerList

type RosterPresenceMap = Map BareJID (Map XMPPResource Presence)

data RosterPresenceEvent
  = FirstResource FullJID Presence
  | NewResource FullJID Presence
  | UpdateResource FullJID Presence
  | RemoveResource FullJID [Element]
  | LastResource FullJID [Element]
  deriving (Show, Eq)

type RosterPresenceSlot m = Slot m RosterPresenceEvent

data RosterPresenceState m = RosterPresenceState
  { rpresenceRef :: IORef RosterPresenceMap
  , rpresencePluginsRef :: XMPPPluginsRef m
  }

rosterUpdate :: FullJID -> ResourceStatus -> RosterPresenceMap -> Maybe (RosterPresenceMap, RosterPresenceEvent)
rosterUpdate full@(FullJID {..}) (ResourceAvailable pres) rmap =
  case M.lookup fullBare rmap of
    Just presences ->
      let updatedRmap = M.insert fullBare (M.insert fullResource pres presences) rmap
       in if M.member fullResource presences
            then Just (updatedRmap, UpdateResource full pres)
            else Just (updatedRmap, NewResource full pres)
    Nothing -> Just (M.insert fullBare (M.singleton fullResource pres) rmap, FirstResource full pres)
rosterUpdate full@(FullJID {..}) (ResourceUnavailable err) rmap =
  case M.lookup fullBare rmap of
    Just presences ->
      if M.member fullResource presences
        then
          if M.size presences == 1
            then Just (M.delete fullBare rmap, LastResource full err)
            else Just (M.insert fullBare (M.delete fullResource presences) rmap, RemoveResource full err)
        else Nothing
    Nothing -> Nothing

rpresencePHandler :: (MonadStream m) => RosterPresenceSlot m -> RosterPresenceState m -> PresenceHandler m
rpresencePHandler slot (RosterPresenceState {..}) (ResourcePresence full presUpd) = do
  roster <- rosterEntries <$> getRoster rpresencePluginsRef
  if not $ bareJidAddress (fullBare full) `M.member` roster
    then return Nothing
    else do
      rpres <- readIORef rpresenceRef
      case rosterUpdate full presUpd rpres of
        Nothing -> return Nothing
        Just (pres', event) -> do
          atomicWriteIORef rpresenceRef pres'
          Slot.call slot event
          return $ Just ()
rpresencePHandler slot (RosterPresenceState {..}) (AllResourcesOffline bare extended) = do
  roster <- rosterEntries <$> getRoster rpresencePluginsRef
  if not $ bareJidAddress bare `M.member` roster
    then return Nothing
    else do
      rpres <- readIORef rpresenceRef
      case M.lookup bare rpres of
        Nothing -> return $ Just ()
        Just resources -> do
          let handleOne pres resource =
                case rosterUpdate (FullJID bare resource) (ResourceUnavailable extended) pres of
                  Nothing -> return pres
                  Just (pres', event) -> do
                    atomicWriteIORef rpresenceRef pres'
                    Slot.call slot event
                    return pres'
          foldM_ handleOne rpres $ M.keys resources
          return $ Just ()

getRosterPresence :: forall m. (MonadStream m) => XMPPPluginsRef m -> m RosterPresenceMap
getRosterPresence pluginsRef = do
  RosterPresenceState {..} <- getPluginsHook (Proxy :: Proxy (RosterPresenceState m)) pluginsRef
  readIORef rpresenceRef

rpresenceSlot :: (MonadStream m) => XMPPPluginsRef m -> m (RosterPresenceSlot m)
rpresenceSlot = getPluginsHook Proxy

rpresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
rpresencePlugin pluginsRef = do
  rpresenceRef <- newIORef M.empty
  let state = RosterPresenceState {rpresencePluginsRef = pluginsRef, ..}
  insertPluginsHook state pluginsRef
  slot <- Slot.new
  insertPluginsHook slot pluginsRef
  pHandlers <- presenceHandlers pluginsRef
  void $ HandlerList.add pHandlers (rpresencePHandler slot state)
