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

import qualified Control.HandlerList as HandlerList
import Control.Slot (Slot)
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

subscriptionInHandler :: (MonadStream m) => SubscriptionSlot m -> SubscriptionState m -> PluginInHandler m
subscriptionInHandler slot _ (InStanza {istType = InPresence (Right (Just typ)), istFrom = Just (bareJidGet -> Just addr)}) =
  case typ of
    PresenceSubscribed -> do
      Slot.call slot (addr, WeSubscribed)
      return $ Just InSilent
    PresenceUnsubscribed -> do
      Slot.call slot (addr, WeUnsubscribed)
      return $ Just InSilent
    PresenceSubscribe -> do
      Slot.call slot (addr, TheyRequested)
      return $ Just InSilent
    PresenceUnsubscribe -> do
      Slot.call slot (addr, TheyUnsubscribed)
      return $ Just InSilent
    _ -> return Nothing
subscriptionInHandler slot (SubscriptionState {..}) (InStanza {istType = InPresence (Left err), istFrom = Just (bareJidGet -> Just addr)}) = do
  found <- atomicModifyIORef' subscriptionPending $ \pending ->
    if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
  if found
    then do
      Slot.call slot (addr, TheyUnavailable err)
      return $ Just InSilent
    else return Nothing
subscriptionInHandler _ _ _ = return Nothing

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

subscriptionRosterEvent :: (MonadStream m) => SubscriptionSlot m -> SubscriptionState m -> (RosterEntries, RosterEvent) -> m ()
subscriptionRosterEvent slot (SubscriptionState {..}) (_, RosterInsert (bareJidGet -> Just addr) _) = do
  found <- atomicModifyIORef' subscriptionPending $ \pending ->
    if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
  when found $ Slot.call slot (addr, TheyReceived)
subscriptionRosterEvent _ _ _ = return ()

subscriptionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
subscriptionPlugin pluginsRef = do
  slot <- Slot.new
  insertPluginsHook slot pluginsRef
  subscriptionPending <- newIORef S.empty
  let state = SubscriptionState {subscriptionSession = pluginsSession pluginsRef, ..}
  insertPluginsHook state pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  void $ HandlerList.add inHandlers $ subscriptionInHandler slot state
  rSlot <- rosterSlot pluginsRef
  void $ Slot.add rSlot $ subscriptionRosterEvent slot state
