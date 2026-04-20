{-# LANGUAGE Strict #-}

{- | XEP-0085: Chat State Notifications
https://xmpp.org/extensions/xep-0085.html
-}
module Network.XMPP.XEP.ChatStates (
  ChatState (..),
  parseChatState,
  ChatStateSlot,
  ChatStatePlugin,
  chatStatePluginSlot,
  getChatStatePlugin,
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
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.XMPP.Address
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
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

chatStatesNS :: Text
chatStateName :: Text -> Name
(chatStatesNS, chatStateName) = namePair "http://jabber.org/protocol/chatstates"

parseChatState :: [Element] -> Maybe ChatState
parseChatState = listToMaybe . mapMaybe tryParse
 where
  tryParse e
    | nameNamespace (elementName e) == Just chatStatesNS = injFrom $ nameLocalName $ elementName e
    | otherwise = Nothing

type ChatStateSlot m = Slot m (XMPPAddress, MessageType, ChatState)

data ChatStatePlugin m = ChatStatePlugin
  { chatStatePluginSession :: StanzaSession m
  , chatStatePluginSlot :: ChatStateSlot m
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
instance (MonadStream m) => SlotSignal m AddressedIMMessage (ChatStatePlugin m) where
  emitSignal (ChatStatePlugin {..}) AddressedIMMessage {imFrom, imMessage} =
    case parseChatState (imRaw imMessage) of
      Just cs -> Slot.call chatStatePluginSlot (imFrom, imType imMessage, cs)
      Nothing -> return ()

hasBody :: [Element] -> Bool
hasBody = any (\e -> elementName e == jcName "body")

getChatStatePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (ChatStatePlugin m)
getChatStatePlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (ChatStatePlugin m)) $ pluginsHooksSet pluginsRef

chatStateSend :: (MonadStream m) => ChatStatePlugin m -> XMPPAddress -> MessageType -> ChatState -> m ()
chatStateSend ChatStatePlugin {chatStatePluginSession} to msgType cs =
  void $
    stanzaSend
      chatStatePluginSession
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage msgType
        , ostChildren = [closedElement $ chatStateName $ injTo cs]
        }

instance (Typeable m) => DiscoInfoProvider (ChatStatePlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton chatStatesNS

chatStatePlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
chatStatePlugin pluginsRef = do
  chatStatePluginSlot <- Slot.new
  let chatStatePluginSession = pluginsSession pluginsRef
      plugin :: ChatStatePlugin m = ChatStatePlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  imp <- getIMPlugin pluginsRef
  Slot.pushNewOrFailM plugin (imPluginSlot imp)
  addDiscoInfo pluginsRef plugin
