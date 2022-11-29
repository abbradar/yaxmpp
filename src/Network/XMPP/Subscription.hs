{-# LANGUAGE Strict #-}

module Network.XMPP.Subscription
  ( SubscriptionStatus(..)
  , SubscriptionRef
  , subscriptionSession
  , requestSubscriptionTo
  , updateSubscriptionFrom
  , subscriptionSlot
  , subscriptionPlugin
  ) where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import UnliftIO.IORef

import Control.Slot (Slot, SlotRef)
import qualified Control.Slot as Slot
import qualified Control.HandlerList as HandlerList
import Network.XMPP.Address
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Roster
import Network.XMPP.Plugin

data SubscriptionStatus = WeSubscribed
                        | WeUnsubscribed
                        | TheyReceived
                        | TheyUnavailable StanzaError
                        | TheyRequested
                        | TheyUnsubscribed
                        deriving (Show, Eq)

data SubscriptionRef m = SubscriptionRef { subscriptionSlotI :: Slot m (BareJID, SubscriptionStatus)
                                         , subscriptionSession :: StanzaSession m
                                         , subscriptionPending :: IORef (Set BareJID)
                                         }

subscriptionInHandler :: MonadStream m => SubscriptionRef m -> PluginInHandler m
subscriptionInHandler (SubscriptionRef {..}) (InStanza { istType = InPresence (Right (Just typ)), istFrom = Just (bareJidGet -> Just addr) }) =
  case typ of
    PresenceSubscribed -> do
      Slot.call subscriptionSlotI (addr, WeSubscribed)
      return $ Just InSilent
    PresenceUnsubscribed -> do
      Slot.call subscriptionSlotI (addr, WeUnsubscribed)
      return $ Just InSilent
    PresenceSubscribe -> do
      Slot.call subscriptionSlotI (addr, TheyRequested)
      return $ Just InSilent
    PresenceUnsubscribe -> do
      Slot.call subscriptionSlotI (addr, TheyUnsubscribed)
      return $ Just InSilent
    _ -> return Nothing
subscriptionInHandler (SubscriptionRef {..}) (InStanza { istType = InPresence (Left err), istFrom = Just (bareJidGet -> Just addr) }) = do
  found <- atomicModifyIORef' subscriptionPending $ \pending ->
    if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
  if found
    then do
      Slot.call subscriptionSlotI (addr, TheyUnavailable err)
      return $ Just InSilent
    else return Nothing
subscriptionInHandler _ _ = return Nothing

requestSubscriptionTo :: MonadStream m => SubscriptionRef m -> BareJID -> Bool -> m ()
requestSubscriptionTo (SubscriptionRef {..}) addr status = do
  atomicModifyIORef' subscriptionPending $ \pending -> (S.insert addr pending, ())
  void $ stanzaSend subscriptionSession OutStanza { ostTo = Just $ bareJidAddress addr
                                                  , ostType = OutPresence $ Just $ if status then PresenceSubscribe else PresenceUnsubscribe
                                                  , ostChildren = []
                                                  }

updateSubscriptionFrom :: MonadStream m => SubscriptionRef m -> BareJID -> Bool -> m ()
updateSubscriptionFrom (SubscriptionRef {..}) addr status =
  void $ stanzaSend subscriptionSession OutStanza { ostTo = Just $ bareJidAddress addr
                                                  , ostType = OutPresence $ Just $ if status then PresenceSubscribed else PresenceUnsubscribed
                                                  , ostChildren = []
                                                  }

subscriptionSlot :: SubscriptionRef m -> SlotRef m (BareJID, SubscriptionStatus)
subscriptionSlot ref = Slot.ref $ subscriptionSlotI ref

subscriptionRosterEvent :: MonadStream m => SubscriptionRef m -> (RosterEntries, RosterEvent) -> m ()
subscriptionRosterEvent (SubscriptionRef {..}) (_, RosterInsert (bareJidGet -> Just addr) _) = do
  found <- atomicModifyIORef' subscriptionPending $ \pending ->
    if addr `S.member` pending then (S.delete addr pending, True) else (pending, False)
  when found $ Slot.call subscriptionSlotI (addr, TheyReceived)
subscriptionRosterEvent _ _ = return ()

subscriptionPlugin :: MonadStream m => XMPPPluginsRef m -> RosterRef m -> m (SubscriptionRef m)
subscriptionPlugin pluginsRef rosterRef = do
  subscriptionSlotI <- Slot.new
  subscriptionPending <- newIORef S.empty
  let subscriptionRef = SubscriptionRef { subscriptionSession = pluginsSession pluginsRef, .. }
  void $ HandlerList.add (pluginInHandlers pluginsRef) $ subscriptionInHandler subscriptionRef
  void $ Slot.add (rosterSlot rosterRef) $ subscriptionRosterEvent subscriptionRef
  return subscriptionRef
