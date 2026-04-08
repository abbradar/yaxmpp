{-# LANGUAGE Strict #-}

module Network.XMPP.Subscription (
  SubscriptionStatus (..),
  requestSubscriptionTo,
  updateSubscriptionFrom,
  subscriptionSlot,
  subscriptionPlugin,
) where

import Control.Monad
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import UnliftIO.IORef

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Slot (Slot, SlotSignal (..))
import qualified Control.Slot as Slot
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Roster
import Network.XMPP.Stanza
import Network.XMPP.Stream

data SubscriptionStatus
  = WeSubscribed
  | WeUnsubscribed
  | TheyReceived
  | TheyUnavailable StanzaError
  | TheyRequested
  | TheyUnsubscribed
  deriving (Show, Eq)

type SubscriptionSlot m = Slot m (BareJID, SubscriptionStatus)

data SubscriptionState m = SubscriptionState
  { subscriptionSession :: StanzaSession m
  , subscriptionPending :: IORef (Set BareJID)
  }

data SubscriptionPlugin m = SubscriptionPlugin
  { subscriptionPluginSlot :: SubscriptionSlot m
  , subscriptionPluginState :: SubscriptionState m
  }

instance (MonadStream m) => Handler m InStanza InResponse (SubscriptionPlugin m) where
  tryHandle (SubscriptionPlugin {..}) (InStanza {istType = InPresence (Right (Just typ)), istFrom = Just (bareJidGet -> Just addr)}) =
    case typ of
      PresenceSubscribed -> do
        Slot.call subscriptionPluginSlot (addr, WeSubscribed)
        return $ Just InSilent
      PresenceUnsubscribed -> do
        Slot.call subscriptionPluginSlot (addr, WeUnsubscribed)
        return $ Just InSilent
      PresenceSubscribe -> do
        Slot.call subscriptionPluginSlot (addr, TheyRequested)
        return $ Just InSilent
      PresenceUnsubscribe -> do
        Slot.call subscriptionPluginSlot (addr, TheyUnsubscribed)
        return $ Just InSilent
      _ -> return Nothing
  tryHandle (SubscriptionPlugin {..}) (InStanza {istType = InPresence (Left err), istFrom = Just (bareJidGet -> Just addr)}) = do
    let SubscriptionState {..} = subscriptionPluginState
    found <- atomicModifyIORef' subscriptionPending $ \pending ->
      if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
    if found
      then do
        Slot.call subscriptionPluginSlot (addr, TheyUnavailable err)
        return $ Just InSilent
      else return Nothing
  tryHandle _ _ = return Nothing

instance (MonadStream m) => SlotSignal m (RosterEntries, RosterEvent) (SubscriptionPlugin m) where
  emitSignal (SubscriptionPlugin {..}) (_, RosterInsert (bareJidGet -> Just addr) _) = do
    let SubscriptionState {..} = subscriptionPluginState
    found <- atomicModifyIORef' subscriptionPending $ \pending ->
      if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
    when found $ Slot.call subscriptionPluginSlot (addr, TheyReceived)
  emitSignal _ _ = return ()

requestSubscriptionTo :: (MonadStream m) => XMPPPluginsRef m -> BareJID -> Bool -> m ()
requestSubscriptionTo pluginsRef addr status = do
  SubscriptionState {..} <- getPluginsHook Proxy pluginsRef
  atomicModifyIORef' subscriptionPending $ \pending -> (S.insert addr pending, ())
  void $
    stanzaSend
      subscriptionSession
      OutStanza
        { ostTo = Just $ bareJidAddress addr
        , ostType = OutPresence $ Just $ if status then PresenceSubscribe else PresenceUnsubscribe
        , ostChildren = []
        }

updateSubscriptionFrom :: (MonadStream m) => XMPPPluginsRef m -> BareJID -> Bool -> m ()
updateSubscriptionFrom pluginsRef addr status = do
  SubscriptionState {..} <- getPluginsHook Proxy pluginsRef
  void $
    stanzaSend
      subscriptionSession
      OutStanza
        { ostTo = Just $ bareJidAddress addr
        , ostType = OutPresence $ Just $ if status then PresenceSubscribed else PresenceUnsubscribed
        , ostChildren = []
        }

-- | Get the subscription slot from the plugins hook set.
subscriptionSlot :: (MonadStream m) => XMPPPluginsRef m -> m (SubscriptionSlot m)
subscriptionSlot = getPluginsHook Proxy

subscriptionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
subscriptionPlugin pluginsRef = do
  subscriptionPluginSlot <- Slot.new
  subscriptionPending <- newIORef S.empty
  let subscriptionPluginState = SubscriptionState {subscriptionSession = pluginsSession pluginsRef, ..}
      plugin :: SubscriptionPlugin m = SubscriptionPlugin {..}
  insertPluginsHook subscriptionPluginSlot pluginsRef
  insertPluginsHook subscriptionPluginState pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  rSlot <- rosterSlot pluginsRef
  Slot.pushNewOrFailM plugin rSlot
