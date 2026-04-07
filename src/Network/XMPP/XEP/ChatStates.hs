{-# LANGUAGE Strict #-}

{- | XEP-0085: Chat State Notifications
https://xmpp.org/extensions/xep-0085.html
-}
module Network.XMPP.XEP.ChatStates (
  ChatState (..),
  parseChatState,
  chatStateSlot,
  chatStateSend,
  chatStatePlugin,
)
where

import qualified Control.HandlerList as HandlerList
import Control.Monad
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Injective
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Network.XMPP.Address
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XML
import Text.XML

data ChatState
  = Active
  | Composing
  | Paused
  | Inactive
  | Gone
  deriving (Show, Eq, Bounded, Enum)

instance Injective ChatState Text where
  injTo s = case s of
    Active -> "active"
    Composing -> "composing"
    Paused -> "paused"
    Inactive -> "inactive"
    Gone -> "gone"

_chatStatesNS :: Text
chatStateName :: Text -> Name
(_chatStatesNS, chatStateName) = namePair "http://jabber.org/protocol/chatstates"

parseChatState :: [Element] -> Maybe ChatState
parseChatState = listToMaybe . mapMaybe tryParse
 where
  tryParse e
    | nameNamespace (elementName e) == Just _chatStatesNS = injFrom $ nameLocalName $ elementName e
    | otherwise = Nothing

type ChatStateSlot m = Slot m (XMPPAddress, MessageType, ChatState)

chatStateInHandler :: (MonadStream m) => ChatStateSlot m -> PluginInHandler m
-- Only handle bodyless messages; messages with a body are handled by the IM plugin,
-- and the chat state can be extracted from IMMessage.imExtended if needed.
chatStateInHandler slot (InStanza {istFrom = Just from, istType = InMessage (Right msgType), istChildren})
  | not (hasBody istChildren)
  , Just cs <- parseChatState istChildren = do
      Slot.call slot (from, msgType, cs)
      return $ Just InSilent
chatStateInHandler _ _ = return Nothing

hasBody :: [Element] -> Bool
hasBody = any (\e -> elementName e == jcName "body")

chatStateSlot :: (MonadStream m) => XMPPPluginsRef m -> m (ChatStateSlot m)
chatStateSlot = getPluginsHook Proxy

chatStateSend :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> MessageType -> ChatState -> m ()
chatStateSend pluginsRef to msgType cs =
  void $
    stanzaSend
      (pluginsSession pluginsRef)
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage msgType
        , ostChildren = [closedElement $ chatStateName $ injTo cs]
        }

chatStateImHandler :: (MonadStream m) => ChatStateSlot m -> (XMPPAddress, IMMessage) -> m ()
chatStateImHandler slot (from, msg) =
  case parseChatState (imExtended msg) of
    Just cs -> Slot.call slot (from, imType msg, cs)
    Nothing -> return ()

chatStatePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
chatStatePlugin pluginsRef = do
  slot <- Slot.new
  insertPluginsHook slot pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  void $ HandlerList.add inHandlers $ chatStateInHandler slot
  -- Also extract chat states from content messages (XEP-0085 §5.3).
  imS <- imSlot pluginsRef
  void $ Slot.add imS $ chatStateImHandler slot
