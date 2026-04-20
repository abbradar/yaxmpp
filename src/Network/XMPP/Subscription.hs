{-# LANGUAGE Strict #-}

module Network.XMPP.Subscription (
  SubscriptionStatus (..),
  SubscriptionSlot,
  SubscriptionPlugin,
  subscriptionPluginSlot,
  getSubscriptionPlugin,
  requestSubscriptionTo,
  updateSubscriptionFrom,
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
import qualified Data.Registry.Mutable as RegRef
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

data SubscriptionPlugin m = SubscriptionPlugin
  { subscriptionPluginSession :: StanzaSession m
  , subscriptionPluginSlot :: SubscriptionSlot m
  , subscriptionPluginPending :: IORef (Set BareJID)
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
    found <- atomicModifyIORef' subscriptionPluginPending $ \pending ->
      if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
    if found
      then do
        Slot.call subscriptionPluginSlot (addr, TheyUnavailable err)
        return $ Just InSilent
      else return Nothing
  tryHandle _ _ = return Nothing

instance (MonadStream m) => SlotSignal m (RosterEntries, RosterEvent) (SubscriptionPlugin m) where
  emitSignal (SubscriptionPlugin {..}) (_, RosterInsert (bareJidGet -> Just addr) _) = do
    found <- atomicModifyIORef' subscriptionPluginPending $ \pending ->
      if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
    when found $ Slot.call subscriptionPluginSlot (addr, TheyReceived)
  emitSignal _ _ = return ()

requestSubscriptionTo :: (MonadStream m) => SubscriptionPlugin m -> BareJID -> Bool -> m ()
requestSubscriptionTo SubscriptionPlugin {..} addr status = do
  atomicModifyIORef' subscriptionPluginPending $ \pending -> (S.insert addr pending, ())
  void $
    stanzaSend
      subscriptionPluginSession
      OutStanza
        { ostTo = Just $ bareJidAddress addr
        , ostType = OutPresence $ Just $ if status then PresenceSubscribe else PresenceUnsubscribe
        , ostChildren = []
        }

updateSubscriptionFrom :: (MonadStream m) => SubscriptionPlugin m -> BareJID -> Bool -> m ()
updateSubscriptionFrom SubscriptionPlugin {subscriptionPluginSession} addr status =
  void $
    stanzaSend
      subscriptionPluginSession
      OutStanza
        { ostTo = Just $ bareJidAddress addr
        , ostType = OutPresence $ Just $ if status then PresenceSubscribed else PresenceUnsubscribed
        , ostChildren = []
        }

-- | Get the subscription plugin from the plugins hook set.
getSubscriptionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (SubscriptionPlugin m)
getSubscriptionPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (SubscriptionPlugin m)) $ pluginsHooksSet pluginsRef

subscriptionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
subscriptionPlugin pluginsRef = do
  subscriptionPluginSlot <- Slot.new
  subscriptionPluginPending <- newIORef S.empty
  let subscriptionPluginSession = pluginsSession pluginsRef
      plugin :: SubscriptionPlugin m = SubscriptionPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  rp <- getRosterPlugin pluginsRef
  Slot.pushNewOrFailM plugin (rosterPluginSlot rp)
