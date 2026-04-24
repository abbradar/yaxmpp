{-# LANGUAGE Strict #-}

{- | XEP-0333: Chat Markers
https://xmpp.org/extensions/xep-0333.html
-}
module Network.XMPP.XEP.ChatMarkers (
  ChatMarker (..),
  ChatMarkerSlot,
  ChatMarkersPlugin,
  chatMarkersPluginSlot,
  getChatMarkersPlugin,
  chatMarkerSend,
  chatMarkersPlugin,
)
where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Maybe
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML
import Text.XML

chatMarkersNS :: Text
chatMarkerName :: Text -> Name
(chatMarkersNS, chatMarkerName) = namePair "urn:xmpp:chat-markers:0"

data ChatMarker
  = Received StanzaId
  | Displayed StanzaId
  | Acknowledged StanzaId
  deriving (Show, Eq)

tryParseChatMarker :: [Element] -> Maybe ChatMarker
tryParseChatMarker = listToMaybe . mapMaybe tryParse
 where
  tryParse e
    | elementName e == chatMarkerName "received" = Received <$> getAttr "id" e
    | elementName e == chatMarkerName "displayed" = Displayed <$> getAttr "id" e
    | elementName e == chatMarkerName "acknowledged" = Acknowledged <$> getAttr "id" e
    | otherwise = Nothing

chatMarkerElement :: ChatMarker -> Element
chatMarkerElement (Received mid) = element (chatMarkerName "received") [("id", mid)] []
chatMarkerElement (Displayed mid) = element (chatMarkerName "displayed") [("id", mid)] []
chatMarkerElement (Acknowledged mid) = element (chatMarkerName "acknowledged") [("id", mid)] []

type ChatMarkerSlot m = Slot m (XMPPAddress, MessageType, ChatMarker)

data ChatMarkersPlugin m = ChatMarkersPlugin
  { chatMarkersPluginSession :: StanzaSession m
  , chatMarkersPluginSlot :: ChatMarkerSlot m
  }

instance (MonadStream m) => Handler m InStanza InResponse (ChatMarkersPlugin m) where
  -- XEP-0333 §4: chat markers target chat/groupchat/normal messages;
  -- error and headline stanzas are out of scope.
  tryHandle (ChatMarkersPlugin {..}) (InStanza {istFrom = Just from, istType = InMessage msgType, istChildren})
    | msgType /= MessageError
    , msgType /= MessageHeadline
    , Just marker <- tryParseChatMarker istChildren = do
        Slot.call chatMarkersPluginSlot (from, msgType, marker)
        return $ Just InSilent
  tryHandle _ _ = return Nothing

getChatMarkersPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (ChatMarkersPlugin m)
getChatMarkersPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (ChatMarkersPlugin m)) $ pluginsHooksSet pluginsRef

chatMarkerSend :: (MonadStream m) => ChatMarkersPlugin m -> XMPPAddress -> MessageType -> ChatMarker -> m ()
chatMarkerSend ChatMarkersPlugin {chatMarkersPluginSession} to msgType marker =
  void $
    stanzaSend
      chatMarkersPluginSession
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage msgType
        , ostChildren = [chatMarkerElement marker]
        }

instance (Typeable m) => DiscoInfoProvider (ChatMarkersPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton chatMarkersNS

chatMarkersPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
chatMarkersPlugin pluginsRef = do
  chatMarkersPluginSlot <- Slot.new
  let chatMarkersPluginSession = pluginsSession pluginsRef
      plugin :: ChatMarkersPlugin m = ChatMarkersPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsInHandlers pluginsRef
  addDiscoInfo pluginsRef plugin
