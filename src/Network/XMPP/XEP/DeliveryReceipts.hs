{-# LANGUAGE Strict #-}

-- | XEP-0184: Message Delivery Receipts
module Network.XMPP.XEP.DeliveryReceipts (
  DeliveryReceiptRequest (..),
  DeliveryReceiptSlot,
  DeliveryReceiptsPlugin,
  deliveryReceiptsPluginSlot,
  requestReceipt,
  getDeliveryReceiptsPlugin,
  deliveryReceiptsPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (Slot, SlotSignal (..))
import qualified Control.Slot as Slot
import Data.List (partition)
import Data.Maybe
import Data.Proxy
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.XML

import Network.XMPP.Address
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML

receiptsNS :: Text
receiptsName :: Text -> Name
(receiptsNS, receiptsName) = namePair "urn:xmpp:receipts"

{- | Marker for 'imExtended': on outgoing messages, set it (via 'requestReceipt')
to attach a @\<request/\>@ child; on incoming messages, its presence means the
sender requested a receipt.
-}
data DeliveryReceiptRequest = DeliveryReceiptRequest
  deriving (Show, Eq)

-- | Slot payload: @(sender, id of the acknowledged message)@.
type DeliveryReceiptSlot m = Slot m (XMPPAddress, MessageId)

data DeliveryReceiptsPlugin m = DeliveryReceiptsPlugin
  { deliveryReceiptsPluginSession :: StanzaSession m
  , deliveryReceiptsPluginSlot :: DeliveryReceiptSlot m
  }

requestElement :: Element
requestElement = closedElement (receiptsName "request")

receivedElement :: MessageId -> Element
receivedElement mid = element (receiptsName "received") [("id", mid)] []

-- | Attach a 'DeliveryReceiptRequest' marker to an 'IMMessage' so the codec emits @\<request/\>@.
requestReceipt :: IMMessage -> IMMessage
requestReceipt msg = msg {imExtended = Reg.insert DeliveryReceiptRequest (imExtended msg)}

extractRequest :: [Element] -> (Maybe DeliveryReceiptRequest, [Element])
extractRequest elems =
  let (reqElems, rest) = partition ((== receiptsName "request") . elementName) elems
   in (DeliveryReceiptRequest <$ listToMaybe reqElems, rest)

extractReceipt :: [Element] -> (Maybe MessageId, [Element])
extractReceipt elems =
  let (recElems, rest) = partition ((== receiptsName "received") . elementName) elems
   in (listToMaybe (mapMaybe (getAttr "id") recElems), rest)

hasBody :: [Element] -> Bool
hasBody = any (\e -> elementName e == jcName "body")

sendReceipt :: (MonadStream m) => DeliveryReceiptsPlugin m -> XMPPAddress -> MessageType -> MessageId -> m ()
sendReceipt DeliveryReceiptsPlugin {deliveryReceiptsPluginSession} to msgType mid =
  void $
    stanzaSend
      deliveryReceiptsPluginSession
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage msgType
        , ostChildren = [receivedElement mid]
        }

-- | Codec witness for moving 'DeliveryReceiptRequest' between 'imExtended' and @\<request/\>@.
data DeliveryReceiptRequestCodec = DeliveryReceiptRequestCodec

instance (MonadStream m) => Codec m XMPPAddress IMMessage DeliveryReceiptRequestCodec where
  codecDecode _ _ msg =
    let (mreq, raw') = extractRequest (imRaw msg)
        ext' = maybe (imExtended msg) (`Reg.insert` imExtended msg) mreq
     in return $ msg {imRaw = raw', imExtended = ext'}
  codecEncode _ _ msg =
    let ext = imExtended msg
        (mreq, ext') = Reg.pop (Proxy :: Proxy DeliveryReceiptRequest) ext
        raw = imRaw msg
        raw' = maybe raw (const (requestElement : raw)) mreq
     in return $ msg {imRaw = raw', imExtended = ext'}

{- | Swallow bodyless message stanzas carrying only @\<request/\>@ or
@\<received/\>@ children so the handler chain does not log them as unhandled.
Side effects happen in the post-in slot subscriber below.
-}
instance (MonadStream m) => Handler m InStanza InResponse (DeliveryReceiptsPlugin m) where
  tryHandle _ (InStanza {istType = InMessage (Right _), istChildren})
    | not (hasBody istChildren)
    , isJust (fst (extractRequest istChildren)) || isJust (fst (extractReceipt istChildren)) =
        return $ Just InSilent
  tryHandle _ _ = return Nothing

{- | Post-in slot subscriber: fires for every incoming stanza. Sends receipts
for @\<request/\>@ children and emits on the slot for @\<received/\>@ children.
-}
instance (MonadStream m) => SlotSignal m InStanza (DeliveryReceiptsPlugin m) where
  emitSignal plugin@DeliveryReceiptsPlugin {deliveryReceiptsPluginSlot} (InStanza {istFrom = Just from, istId, istType = InMessage (Right msgType), istChildren}) = do
    when (isJust $ fst $ extractRequest istChildren) $
      mapM_ (sendReceipt plugin from msgType) istId
    case fst (extractReceipt istChildren) of
      Just mid -> Slot.call deliveryReceiptsPluginSlot (from, mid)
      Nothing -> return ()
  emitSignal _ _ = return ()

instance (Typeable m) => DiscoInfoProvider (DeliveryReceiptsPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton receiptsNS

getDeliveryReceiptsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (DeliveryReceiptsPlugin m)
getDeliveryReceiptsPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (DeliveryReceiptsPlugin m)) $ pluginsHooksSet pluginsRef

deliveryReceiptsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
deliveryReceiptsPlugin pluginsRef = do
  deliveryReceiptsPluginSlot <- Slot.new
  let deliveryReceiptsPluginSession = pluginsSession pluginsRef
      plugin :: DeliveryReceiptsPlugin m = DeliveryReceiptsPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsInHandlers pluginsRef
  imp <- getIMPlugin pluginsRef
  Codec.pushNewOrFailM DeliveryReceiptRequestCodec (imPluginCodecs imp)
  Slot.pushNewOrFailM plugin $ postInSlot pluginsRef
  addDiscoInfo pluginsRef plugin
