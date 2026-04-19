{-# LANGUAGE Strict #-}

-- | XEP-0280: Message Carbons
module Network.XMPP.XEP.Carbons (
  CarbonDirection (..),
  carbonSlot,
  carbonsEnable,
  carbonsDisable,
  carbonsPlugin,
) where

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
import Text.XML

import Data.Injective (injFrom)

import Network.XMPP.Address
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.Forwarding
import Network.XMPP.XML

carbonsNS :: Text
carbonsName :: Text -> Name
(carbonsNS, carbonsName) = namePair "urn:xmpp:carbons:2"

-- | Direction of a carbon-forwarded message relative to the own account.
data CarbonDirection = CarbonReceived | CarbonSent
  deriving (Show, Eq, Enum, Bounded)

-- | Slot payload: (direction, peer address, message).
-- The peer address is the inner message's @from@ for 'CarbonReceived',
-- and the inner message's @to@ for 'CarbonSent'.
type CarbonSlot m = Slot m (CarbonDirection, XMPPAddress, IMMessage)

data CarbonsPlugin m = CarbonsPlugin
  { cpPluginsRef :: XMPPPluginsRef m
  , cpSlot :: CarbonSlot m
  }

parseCarbonDirection :: Name -> Maybe CarbonDirection
parseCarbonDirection n
  | n == carbonsName "received" = Just CarbonReceived
  | n == carbonsName "sent" = Just CarbonSent
  | otherwise = Nothing

-- | If the children contain a carbon wrapper, return its direction and inner
-- forwarded element (or a parse error).
extractCarbon :: [Element] -> Maybe (Either StanzaError (CarbonDirection, Forwarded))
extractCarbon = listToMaybe . mapMaybe tryOne
 where
  tryOne e = do
    direction <- parseCarbonDirection (elementName e)
    let fwdElems = mapMaybe nodeElement (elementNodes e)
    Just $ case listToMaybe $ mapMaybe parseForwarded fwdElems of
      Nothing -> Left $ badRequest "no <forwarded> in carbon wrapper"
      Just (Left err) -> Left err
      Just (Right fwd) -> Right (direction, fwd)

  nodeElement (NodeElement ne) = Just ne
  nodeElement _ = Nothing

-- | Parse the inner @\<message\>@ element carried by the carbon.
parseInnerMessage :: CarbonDirection -> Element -> Either StanzaError (XMPPAddress, MessageType, [Element])
parseInnerMessage direction e = do
  unless (elementName e == jcName "message") $
    Left $ badRequest "forwarded element is not a <message>"
  msgType <- case getAttr "type" e of
    Nothing -> Right MessageNormal
    Just t -> case injFrom t of
      Just mt -> Right mt
      Nothing -> Left $ badRequest "invalid forwarded message type"
  let peerAttr = case direction of
        CarbonReceived -> "from"
        CarbonSent -> "to"
  peerText <- case getAttr peerAttr e of
    Nothing -> Left $ badRequest "forwarded message missing peer address"
    Just v -> Right v
  peer <- case xmppAddress peerText of
    Left _ -> Left $ jidMalformed peerText
    Right a -> Right a
  let children = mapMaybe toElem (elementNodes e)
      toElem (NodeElement ne) = Just ne
      toElem _ = Nothing
  return (peer, msgType, children)

instance (MonadStream m) => Handler m InStanza InResponse (CarbonsPlugin m) where
  tryHandle (CarbonsPlugin {..}) (InStanza {istFrom, istType = InMessage (Right _), istChildren})
    | Just res <- extractCarbon istChildren = Just <$> do
        if not (fromServerOrMyself istFrom (pluginsSession cpPluginsRef))
          then return $ InError $ badRequest "untrusted carbon sender"
          else case res of
            Left err -> return $ InError err
            Right (direction, Forwarded {fwdMessage}) ->
              case parseInnerMessage direction fwdMessage of
                Left err -> return $ InError err
                Right (peer, msgType, children) -> do
                  mMsg <- parseIMMessage cpPluginsRef peer msgType children
                  case mMsg of
                    Nothing -> return InSilent
                    Just (Left err) -> return $ InError err
                    Just (Right msg) -> do
                      Slot.call cpSlot (direction, peer, msg)
                      return InSilent
  tryHandle _ _ = return Nothing

-- | Get the carbons slot from the plugins hook set.
carbonSlot :: (MonadStream m) => XMPPPluginsRef m -> m (CarbonSlot m)
carbonSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

carbonsSet :: (MonadStream m) => XMPPPluginsRef m -> Text -> (Either StanzaError () -> m ()) -> m ()
carbonsSet pluginsRef localName handler =
  stanzaRequest
    (pluginsSession pluginsRef)
    (serverRequest IQSet [closedElement (carbonsName localName)])
    $ \resp -> handler $ case resp of
      Left e -> Left e
      Right _ -> Right ()

-- | Enable message carbons on this session via IQ-set.
carbonsEnable :: (MonadStream m) => XMPPPluginsRef m -> (Either StanzaError () -> m ()) -> m ()
carbonsEnable pluginsRef = carbonsSet pluginsRef "enable"

-- | Disable message carbons on this session via IQ-set.
carbonsDisable :: (MonadStream m) => XMPPPluginsRef m -> (Either StanzaError () -> m ()) -> m ()
carbonsDisable pluginsRef = carbonsSet pluginsRef "disable"

data CarbonsDisco = CarbonsDisco

instance DiscoInfoProvider CarbonsDisco where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton carbonsNS

carbonsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
carbonsPlugin pluginsRef = do
  cpSlot <- Slot.new
  let plugin :: CarbonsPlugin m = CarbonsPlugin {cpPluginsRef = pluginsRef, cpSlot = cpSlot}
  RegRef.insertNewOrFailM cpSlot $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
  addDiscoInfo pluginsRef CarbonsDisco
