module Network.XMPP.Subscription
  ( SubscriptionStatus(..)
  , SubscriptionRef
  , subscriptionSession
  , subSubscribe
  , requestSubscription
  , subscriptionPlugin
  ) where

import Data.Maybe
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

data SubscriptionRef m = SubscriptionRef { subscriptionSignal :: Signal m (XMPPAddress, SubscriptionStatus)
                                         , subscriptionSession :: StanzaSession m
                                         , subscriptionHandler :: XMPPAddress -> m (Maybe Bool)
                                         }

subscriptionInHandler :: MonadSession m => SubscriptionRef m -> PluginInHandler m
subscriptionInHandler (SubscriptionRef {..}) (InStanza { istType = InPresence (Right (Just typ)), istFrom = Just addr })
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
          Just r -> void $ stanzaSend subscriptionSession OutStanza { ostTo = Just addr
                                                                   , ostType = OutPresence $ Just $ if r then PresenceSubscribed else PresenceUnsubscribed
                                                                   , ostChildren = []
                                                                   }
          Nothing -> return ()
      return $ Just Nothing
  | typ == PresenceUnsubscribe = do
      Signal.emit subscriptionSignal (addr, TheyUnsubscribed)
      return $ Just Nothing
subscriptionInHandler _ _ = return Nothing

subSubscribe :: MonadSession m => SubscriptionRef m -> ((XMPPAddress, SubscriptionStatus) -> m ()) -> m ()
subSubscribe (SubscriptionRef {..}) = Signal.subscribe subscriptionSignal

requestSubscription :: MonadSession m => SubscriptionRef m -> XMPPAddress -> m ()
requestSubscription (SubscriptionRef {..}) addr
  | isNothing $ addressResource addr =
      void $ stanzaSend subscriptionSession OutStanza { ostTo = Just addr
                                                      , ostType = OutPresence $ Just PresenceSubscribe
                                                      , ostChildren = []
                                                      }
  | otherwise = fail "requestSubscription: resource shouldn't be specified"

subscriptionPlugin :: MonadSession m => StanzaSession m -> (XMPPAddress -> m (Maybe Bool)) -> m (XMPPPlugin m, SubscriptionRef m)
subscriptionPlugin subscriptionSession subscriptionHandler = do
  subscriptionSignal <- Signal.empty
  let subscriptionRef = SubscriptionRef {..}
      plugin = def { pluginInHandler = subscriptionInHandler subscriptionRef }
  return (plugin, subscriptionRef)
