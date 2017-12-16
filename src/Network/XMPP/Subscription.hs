module Network.XMPP.Subscription
  ( SubscriptionStatus(..)
  , SubscriptionReqHandler
  , SubscriptionRef
  , subscriptionSession
  , requestSubscription
  , subscriptionSetHandler
  , subscriptionPlugin
  ) where

import Control.Monad
import Control.Concurrent.Lifted
import Data.Default.Class

import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.Address
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Plugin

data SubscriptionStatus = WeSubscribed
                        | WeUnsubscribed
                        | TheyUnsubscribed
                        deriving (Show, Eq)

type SubscriptionReqHandler m = BareJID -> m (Maybe Bool)

data SubscriptionRef m = SubscriptionRef { subscriptionHandler :: Handler m (BareJID, SubscriptionStatus)
                                         , subscriptionSession :: StanzaSession m
                                         , subscriptionReqHandler :: SubscriptionReqHandler m
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
      _ <- fork $ do
        mr <- subscriptionReqHandler addr
        case mr of
          Just r -> void $ stanzaSend subscriptionSession OutStanza { ostTo = Just $ bareJidAddress addr
                                                                   , ostType = OutPresence $ Just $ if r then PresenceSubscribed else PresenceUnsubscribed
                                                                   , ostChildren = []
                                                                   }
          Nothing -> return ()
      return $ Just Nothing
  | typ == PresenceUnsubscribe = do
      Handler.call subscriptionHandler (addr, TheyUnsubscribed)
      return $ Just Nothing
subscriptionInHandler _ _ = return Nothing

requestSubscription :: MonadStream m => SubscriptionRef m -> BareJID -> m ()
requestSubscription (SubscriptionRef {..}) addr =
  void $ stanzaSend subscriptionSession OutStanza { ostTo = Just $ bareJidAddress addr
                                                  , ostType = OutPresence $ Just PresenceSubscribe
                                                  , ostChildren = []
                                                  }

subscriptionSetHandler :: MonadStream m => SubscriptionRef m -> ((BareJID, SubscriptionStatus) -> m ()) -> m ()
subscriptionSetHandler (SubscriptionRef {..}) = Handler.set subscriptionHandler

subscriptionPlugin :: MonadStream m => StanzaSession m -> SubscriptionReqHandler m -> m (XMPPPlugin m, SubscriptionRef m)
subscriptionPlugin subscriptionSession subscriptionReqHandler = do
  subscriptionHandler <- Handler.new
  let subscriptionRef = SubscriptionRef {..}
      plugin = def { pluginInHandler = subscriptionInHandler subscriptionRef }
  return (plugin, subscriptionRef)
