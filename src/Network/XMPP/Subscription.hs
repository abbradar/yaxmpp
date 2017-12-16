module Network.XMPP.Subscription
  ( SubscriptionStatus(..)
  , SubscriptionRef
  , subscriptionSession
  , requestSubscriptionTo
  , updateSubscriptionFrom
  , subscriptionSetHandler
  , subscriptionPlugin
  ) where

import Control.Monad
import Data.Default.Class

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.Address
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Plugin

data SubscriptionStatus = WeSubscribed
                        | WeUnsubscribed
                        | TheyRequested
                        | TheyUnsubscribed
                        deriving (Show, Eq)

data SubscriptionRef m = SubscriptionRef { subscriptionHandler :: Handler m (BareJID, SubscriptionStatus)
                                         , subscriptionSession :: StanzaSession m
                                         }

subscriptionInHandler :: MonadStream m => SubscriptionRef m -> PluginInHandler m
subscriptionInHandler (SubscriptionRef {..}) (InStanza { istType = InPresence (Right (Just typ)), istFrom = Just (bareJidGet -> Just addr) })
  | typ == PresenceSubscribed = do
      Handler.call subscriptionHandler (addr, WeSubscribed)
      return $ Just Nothing
  | typ == PresenceUnsubscribed = do
      Handler.call subscriptionHandler (addr, WeUnsubscribed)
      return $ Just Nothing
  | typ == PresenceSubscribe = do
      Handler.call subscriptionHandler (addr, TheyRequested)
      return $ Just Nothing
  | typ == PresenceUnsubscribe = do
      Handler.call subscriptionHandler (addr, TheyUnsubscribed)
      return $ Just Nothing
subscriptionInHandler _ _ = return Nothing

requestSubscriptionTo :: MonadStream m => SubscriptionRef m -> BareJID -> Bool -> m ()
requestSubscriptionTo (SubscriptionRef {..}) addr status =
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

subscriptionSetHandler :: MonadStream m => SubscriptionRef m -> ((BareJID, SubscriptionStatus) -> m ()) -> m ()
subscriptionSetHandler (SubscriptionRef {..}) = Handler.set subscriptionHandler

subscriptionPlugin :: MonadStream m => StanzaSession m -> m (XMPPPlugin m, SubscriptionRef m)
subscriptionPlugin subscriptionSession = do
  subscriptionHandler <- Handler.new
  let subscriptionRef = SubscriptionRef {..}
      plugin = def { pluginInHandler = subscriptionInHandler subscriptionRef }
  return (plugin, subscriptionRef)
