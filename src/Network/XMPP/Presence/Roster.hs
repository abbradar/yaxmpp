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

type RosterPresenceMap m = Map BareJID (Map XMPPResource (PresenceRef m))

data RosterPresenceEvent m
  = FirstResource FullJID (PresenceRef m)
  | NewResource FullJID (PresenceRef m)
  | UpdateResource FullJID (PresenceRef m)
  | RemoveResource FullJID [Element]
  | LastResource FullJID [Element]
  deriving (Show)

type RosterPresenceSlot m = Slot m (RosterPresenceEvent m)

data RosterPresencePlugin m = RosterPresencePlugin
  { rpresencePluginRoster :: RosterPlugin m
  , rpresencePluginSlot :: RosterPresenceSlot m
  , rpresencePluginRef :: IORef (RosterPresenceMap m)
  }

rosterUpdate :: FullJID -> ResourceStatus m -> RosterPresenceMap m -> Maybe (RosterPresenceMap m, RosterPresenceEvent m)
rosterUpdate full@(FullJID {..}) (ResourceAvailable presRef) rmap =
  case M.lookup fullBare rmap of
    Just presences ->
      let updatedRmap = M.insert fullBare (M.insert fullResource presRef presences) rmap
       in if M.member fullResource presences
            then Just (updatedRmap, UpdateResource full presRef)
            else Just (updatedRmap, NewResource full presRef)
    Nothing -> Just (M.insert fullBare (M.singleton fullResource presRef) rmap, FirstResource full presRef)
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

instance (MonadStream m) => Handler m (PresenceUpdate m) () (RosterPresencePlugin m) where
  tryHandle (RosterPresencePlugin {..}) (ResourcePresence full presUpd) = do
    rpres <- readIORef rpresencePluginRef
    track <-
      if fullBare full `M.member` rpres
        then return True
        else case presUpd of
          ResourceAvailable _ ->
            tryGetRoster rpresencePluginRoster >>= \case
              Just (Right (rosters -> roster)) ->
                return $ toXMPPAddress (fullBare full) `M.member` roster
              _ -> return False
          ResourceUnavailable _ -> return False
    if not track
      then return Nothing
      else case rosterUpdate full presUpd rpres of
        Nothing -> return Nothing
        Just (pres', event) -> do
          atomicWriteIORef rpresencePluginRef pres'
          Slot.call rpresencePluginSlot event
          return $ Just ()
  tryHandle (RosterPresencePlugin {..}) (AllResourcesOffline bare extended) = do
    rpres <- readIORef rpresencePluginRef
    case M.lookup bare rpres of
      Nothing -> return Nothing
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

getRosterPresence :: (MonadStream m) => RosterPresencePlugin m -> m (RosterPresenceMap m)
getRosterPresence RosterPresencePlugin {rpresencePluginRef} = readIORef rpresencePluginRef

rpresencePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
rpresencePlugin pluginsRef = do
  rpresencePluginRoster <- getRosterPlugin pluginsRef
  rpresencePluginRef <- newIORef M.empty
  rpresencePluginSlot <- Slot.new
  pp <- getPresencePlugin pluginsRef
  let plugin :: RosterPresencePlugin m = RosterPresencePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ presencePluginHandlers pp
