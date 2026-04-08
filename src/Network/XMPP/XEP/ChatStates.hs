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

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (Slot, SlotSignal (..))
import qualified Control.Slot as Slot
import Data.Injective
import Data.Maybe
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
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

data ChatStatePlugin m = ChatStatePlugin
  { chatStatePluginSlot :: ChatStateSlot m
  }

-- Handle bodyless messages with chat state notifications.
instance (MonadStream m) => Handler m InStanza InResponse (ChatStatePlugin m) where
  tryHandle (ChatStatePlugin {..}) (InStanza {istFrom = Just from, istType = InMessage (Right msgType), istChildren})
    | not (hasBody istChildren)
    , Just cs <- parseChatState istChildren = do
        Slot.call chatStatePluginSlot (from, msgType, cs)
        return $ Just InSilent
  tryHandle _ _ = return Nothing

-- Extract chat states from body-ful messages (XEP-0085 §5.3).
instance (MonadStream m) => SlotSignal m (XMPPAddress, IMMessage) (ChatStatePlugin m) where
  emitSignal (ChatStatePlugin {..}) (from, msg) =
    case parseChatState (imRaw msg) of
      Just cs -> Slot.call chatStatePluginSlot (from, imType msg, cs)
      Nothing -> return ()

hasBody :: [Element] -> Bool
hasBody = any (\e -> elementName e == jcName "body")

chatStateSlot :: (MonadStream m) => XMPPPluginsRef m -> m (ChatStateSlot m)
chatStateSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

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

chatStatePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
chatStatePlugin pluginsRef = do
  chatStatePluginSlot <- Slot.new
  let plugin :: ChatStatePlugin m = ChatStatePlugin {..}
  RegRef.insertNewOrFailM chatStatePluginSlot $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  imS <- imSlot pluginsRef
  Slot.pushNewOrFailM plugin imS
