module Network.XMPP.Subscription
  ( SubscriptionStatus(..)
  , SubscriptionRef
  , subscriptionSession
  , subSubscribe
  , requestSubscription
  , subscriptionPlugin
  ) where

import Control.Monad
import Control.Concurrent.Lifted
import Data.Default.Class

import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Network.XMPP.Address
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin

data SubscriptionStatus = WeSubscribed
                        | WeUnsubscribed
                        | TheyUnsubscribed
                        deriving (Show, Eq)

data SubscriptionRef m = SubscriptionRef { subscriptionSignal :: Signal m (BareJID, SubscriptionStatus)
                                         , subscriptionSession :: StanzaSession m
                                         , subscriptionHandler :: BareJID -> m (Maybe Bool)
                                         }

subscriptionInHandler :: MonadSession m => SubscriptionRef m -> PluginInHandler m
subscriptionInHandler (SubscriptionRef {..}) (InStanza { istType = InPresence (Right (Just typ)), istFrom = Just (bareJidGet -> Just addr) })
  | typ == PresenceSubscribed = do
      Signal.emit subscriptionSignal (addr, WeSubscribed)
      return $ Just Nothing
  | typ == PresenceUnsubscribed = do
      Signal.emit subscriptionSignal (addr, WeUnsubscribed)
      return $ Just Nothing
  | typ == PresenceSubscribe = do
      _ <- fork $ do
        mr <- subscriptionHandler addr
        case mr of
          Just r -> void $ stanzaSend subscriptionSession OutStanza { ostTo = Just $ bareJidAddress addr
                                                                   , ostType = OutPresence $ Just $ if r then PresenceSubscribed else PresenceUnsubscribed
                                                                   , ostChildren = []
                                                                   }
          Nothing -> return ()
      return $ Just Nothing
  | typ == PresenceUnsubscribe = do
      Signal.emit subscriptionSignal (addr, TheyUnsubscribed)
      return $ Just Nothing
subscriptionInHandler _ _ = return Nothing

subSubscribe :: MonadSession m => SubscriptionRef m -> ((BareJID, SubscriptionStatus) -> m ()) -> m ()
subSubscribe (SubscriptionRef {..}) = Signal.subscribe subscriptionSignal

requestSubscription :: MonadSession m => SubscriptionRef m -> BareJID -> m ()
requestSubscription (SubscriptionRef {..}) addr =
  void $ stanzaSend subscriptionSession OutStanza { ostTo = Just $ bareJidAddress addr
                                                  , ostType = OutPresence $ Just PresenceSubscribe
                                                  , ostChildren = []
                                                  }

subscriptionPlugin :: MonadSession m => StanzaSession m -> (BareJID -> m (Maybe Bool)) -> m (XMPPPlugin m, SubscriptionRef m)
subscriptionPlugin subscriptionSession subscriptionHandler = do
  subscriptionSignal <- Signal.empty
  let subscriptionRef = SubscriptionRef {..}
      plugin = def { pluginInHandler = subscriptionInHandler subscriptionRef }
  return (plugin, subscriptionRef)
