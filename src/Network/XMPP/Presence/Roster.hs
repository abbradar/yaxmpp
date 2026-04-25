{-# LANGUAGE Strict #-}

module Network.XMPP.Presence.Roster (
  RosterPresenceMap,
  RosterPresenceEvent (..),
  RosterPresenceSlot,
  RosterPresencePlugin,
  rpresencePluginSlot,
  getRosterPresencePlugin,
  getRosterPresence,
  rpresencePlugin,
) where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Roster
import Network.XMPP.Stream
import Text.XML
import UnliftIO.IORef

type RosterPresenceMap = Map BareJID (Map XMPPResource Presence)

data RosterPresenceEvent
  = FirstResource FullJID Presence
  | NewResource FullJID Presence
  | UpdateResource FullJID Presence
  | RemoveResource FullJID [Element]
  | LastResource FullJID [Element]
  deriving (Show)

type RosterPresenceSlot m = Slot m RosterPresenceEvent

data RosterPresencePlugin m = RosterPresencePlugin
  { rpresencePluginRoster :: RosterPlugin m
  , rpresencePluginSlot :: RosterPresenceSlot m
  , rpresencePluginRef :: IORef RosterPresenceMap
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

instance (MonadStream m) => Handler m PresenceUpdate () (RosterPresencePlugin m) where
  tryHandle (RosterPresencePlugin {..}) (ResourcePresence full presUpd) = do
    roster <- rosterEntries <$> getRoster rpresencePluginRoster
    if not $ toXMPPAddress (fullBare full) `M.member` roster
      then return Nothing
      else do
        rpres <- readIORef rpresencePluginRef
        case rosterUpdate full presUpd rpres of
          Nothing -> return Nothing
          Just (pres', event) -> do
            atomicWriteIORef rpresencePluginRef pres'
            Slot.call rpresencePluginSlot event
            return $ Just ()
  tryHandle (RosterPresencePlugin {..}) (AllResourcesOffline bare extended) = do
    roster <- rosterEntries <$> getRoster rpresencePluginRoster
    if not $ toXMPPAddress bare `M.member` roster
      then return Nothing
      else do
        rpres <- readIORef rpresencePluginRef
        case M.lookup bare rpres of
          Nothing -> return $ Just ()
          Just resources -> do
            let handleOne pres resource =
                  case rosterUpdate (FullJID bare resource) (ResourceUnavailable extended) pres of
                    Nothing -> return pres
                    Just (pres', event) -> do
                      atomicWriteIORef rpresencePluginRef pres'
                      Slot.call rpresencePluginSlot event
                      return pres'
            foldM_ handleOne rpres $ M.keys resources
            return $ Just ()

getRosterPresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (RosterPresencePlugin m)
getRosterPresencePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (RosterPresencePlugin m)) $ pluginsHooksSet pluginsRef

getRosterPresence :: (MonadStream m) => RosterPresencePlugin m -> m RosterPresenceMap
getRosterPresence RosterPresencePlugin {rpresencePluginRef} = readIORef rpresencePluginRef

rpresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
rpresencePlugin pluginsRef = do
  rpresencePluginRef <- newIORef M.empty
  rpresencePluginSlot <- Slot.new
  rpresencePluginRoster <- getRosterPlugin pluginsRef
  pp <- getPresencePlugin pluginsRef
  let plugin :: RosterPresencePlugin m = RosterPresencePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin (presencePluginHandlers pp)
