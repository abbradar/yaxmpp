{-# LANGUAGE Strict #-}

{- | XEP-0333: Chat Markers
https://xmpp.org/extensions/xep-0333.html
-}
module Network.XMPP.XEP.ChatMarkers (
  ChatMarker (..),
  chatMarkerSlot,
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
  = Received MessageId
  | Displayed MessageId
  | Acknowledged MessageId
  deriving (Show, Eq)

parseChatMarker :: [Element] -> Maybe ChatMarker
parseChatMarker = listToMaybe . mapMaybe tryParse
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

newtype ChatMarkersPlugin m = ChatMarkersPlugin
  { chatMarkersPluginSlot :: ChatMarkerSlot m
  }

instance (MonadStream m) => Handler m InStanza InResponse (ChatMarkersPlugin m) where
  tryHandle (ChatMarkersPlugin {..}) (InStanza {istFrom = Just from, istType = InMessage (Right msgType), istChildren})
    | Just marker <- parseChatMarker istChildren = do
        Slot.call chatMarkersPluginSlot (from, msgType, marker)
        return $ Just InSilent
  tryHandle _ _ = return Nothing

chatMarkerSlot :: (MonadStream m) => XMPPPluginsRef m -> m (ChatMarkerSlot m)
chatMarkerSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

chatMarkerSend :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> MessageType -> ChatMarker -> m ()
chatMarkerSend pluginsRef to msgType marker =
  void $
    stanzaSend
      (pluginsSession pluginsRef)
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage msgType
        , ostChildren = [chatMarkerElement marker]
        }

data ChatMarkersDisco = ChatMarkersDisco

instance DiscoInfoProvider ChatMarkersDisco where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton chatMarkersNS

chatMarkersPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
chatMarkersPlugin pluginsRef = do
  chatMarkersPluginSlot <- Slot.new
  let plugin :: ChatMarkersPlugin m = ChatMarkersPlugin {..}
  RegRef.insertNewOrFailM chatMarkersPluginSlot $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  addDiscoInfo pluginsRef ChatMarkersDisco
