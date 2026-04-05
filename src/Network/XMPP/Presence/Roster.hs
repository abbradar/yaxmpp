{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Roster (
  RosterPresenceMap,
  RosterPresenceRef,
  RosterPresenceEvent (..),
  getRosterPresence,
  rpresenceSlot,
  rpresencePlugin,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.XML
import UnliftIO.IORef

import Control.Slot (Slot, SlotRef)
import qualified Control.Slot as Slot
import Network.XMPP.Address
import Network.XMPP.Presence
import Network.XMPP.Roster
import Network.XMPP.Stream

type RosterPresenceMap = Map BareJID (Map XMPPResource Presence)

data RosterPresenceEvent
  = FirstResource FullJID Presence
  | NewResource FullJID Presence
  | UpdateResource FullJID Presence
  | RemoveResource FullJID [Element]
  | LastResource FullJID [Element]
  deriving (Show, Eq)

data RosterPresenceRef m = RosterPresenceRef
  { rpresenceRef :: IORef RosterPresenceMap
  , rpresenceRoster :: RosterRef m
  , rpresenceHandler :: Slot m RosterPresenceEvent
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

rpresencePHandler :: (MonadStream m) => RosterPresenceRef m -> PresenceHandler m
rpresencePHandler (RosterPresenceRef {..}) (ResourcePresence full presUpd) = do
  roster <- rosterEntries <$> getRoster rpresenceRoster
  if bareJidAddress (fullBare full) `M.member` roster
    then do
      rpres <- readIORef rpresenceRef
      case rosterUpdate full presUpd rpres of
        Nothing -> return Nothing
        Just (pres', event) -> do
          atomicWriteIORef rpresenceRef pres'
          Slot.call rpresenceHandler event
          return $ Just ()
    else return Nothing
rpresencePHandler (RosterPresenceRef {..}) (AllResourcesOffline bare extended) = do
  roster <- rosterEntries <$> getRoster rpresenceRoster
  if bareJidAddress bare `M.member` roster
    then do
      rpres <- readIORef rpresenceRef
      case M.lookup bare rpres of
        Nothing -> return Nothing
        Just resources -> case M.toDescList resources of
          [] -> return Nothing
          (lastKey, _) : rest -> do
            let removeEvents = [RemoveResource (FullJID bare k) extended | (k, _) <- rest]
                lastEvent = LastResource (FullJID bare lastKey) extended
            atomicWriteIORef rpresenceRef $ M.delete bare rpres
            mapM_ (Slot.call rpresenceHandler) (removeEvents ++ [lastEvent])
            return $ Just ()
    else return Nothing

getRosterPresence :: (MonadStream m) => RosterPresenceRef m -> m RosterPresenceMap
getRosterPresence = readIORef . rpresenceRef

rpresenceSlot :: RosterPresenceRef m -> SlotRef m RosterPresenceEvent
rpresenceSlot (RosterPresenceRef {..}) = Slot.ref rpresenceHandler

rpresencePlugin :: (MonadStream m) => RosterRef m -> m (PresenceHandler m, RosterPresenceRef m)
rpresencePlugin rpresenceRoster = do
  rpresenceRef <- newIORef M.empty
  rpresenceHandler <- Slot.new
  let pref = RosterPresenceRef {..}
      phandler = rpresencePHandler pref
  return (phandler, pref)
