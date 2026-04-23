{-# LANGUAGE Strict #-}

-- | XEP-0280: Message Carbons
module Network.XMPP.XEP.Carbons (
  CarbonDirection (..),
  CarbonSlot,
  CarbonsPlugin,
  carbonsPluginSlot,
  getCarbonsPlugin,
  carbonsEnable,
  carbonsDisable,
  carbonsPlugin,
) where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.MemoAsync (MemoAsync)
import qualified Control.MemoAsync as MemoAsync
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Maybe
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.XML

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

{- | Slot payload: (direction, addressed message).
For 'CarbonReceived' the inner message's @from@ is the peer and @to@ is us;
for 'CarbonSent' the inner message's @from@ is us and @to@ is the peer.
-}
type CarbonSlot m = Slot m (CarbonDirection, AddressedIMMessage)

data CarbonsPlugin m = CarbonsPlugin
  { carbonsPluginSession :: StanzaSession m
  , carbonsPluginIMPlugin :: IMPlugin m
  , carbonsPluginSupported :: MemoAsync m Bool
  , carbonsPluginSlot :: CarbonSlot m
  }

parseCarbonDirection :: Name -> Maybe CarbonDirection
parseCarbonDirection n
  | n == carbonsName "received" = Just CarbonReceived
  | n == carbonsName "sent" = Just CarbonSent
  | otherwise = Nothing

{- | If the children contain a carbon wrapper, return its direction and inner
forwarded element (or a parse error).
-}
extractCarbon :: [Element] -> Either StanzaError (Maybe (CarbonDirection, Forwarded))
extractCarbon elems = case mapMaybe tryOne elems of
  [] -> Right Nothing
  (res : _) -> res
 where
  tryOne e = do
    direction <- parseCarbonDirection (elementName e)
    let fwdElems = mapMaybe nodeElement (elementNodes e)
    Just $ case mapMaybe (toMaybeForwarded . parseForwarded) fwdElems of
      [] -> Left $ badRequest "no <forwarded> in carbon wrapper"
      (Left err : _) -> Left err
      (Right fwd : _) -> Right $ Just (direction, fwd)

  toMaybeForwarded (Left err) = Just (Left err)
  toMaybeForwarded (Right Nothing) = Nothing
  toMaybeForwarded (Right (Just fwd)) = Just (Right fwd)

  nodeElement (NodeElement ne) = Just ne
  nodeElement _ = Nothing

instance (MonadStream m) => Handler m InStanza InResponse (CarbonsPlugin m) where
  tryHandle (CarbonsPlugin {..}) (InStanza {istFrom, istType = InMessage (Right _), istChildren})
    | Right (Just res) <- extracted =
        Just <$> do
          if not (fromServerOrMyself istFrom carbonsPluginSession)
            then return $ InError $ badRequest "untrusted carbon sender"
            else do
              let (direction, Forwarded {fwdMessage}) = res
              case parseInStanza fwdMessage of
                Left err -> return $ InError err
                Right st@(InStanza {istType = InMessage (Right _)}) -> do
                  mMsg <- parseIMMessage carbonsPluginIMPlugin st
                  case mMsg of
                    Left err -> return $ InError err
                    Right Nothing -> return InSilent
                    Right (Just addressed) -> do
                      Slot.call carbonsPluginSlot (direction, addressed)
                      return InSilent
                Right _ -> return InSilent
    | Left err <- extracted = return $ Just $ InError err
   where
    extracted = extractCarbon istChildren
  tryHandle _ _ = return Nothing

getCarbonsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (CarbonsPlugin m)
getCarbonsPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (CarbonsPlugin m)) $ pluginsHooksSet pluginsRef

carbonsSet :: (MonadStream m) => CarbonsPlugin m -> Text -> (Either StanzaError () -> m ()) -> m ()
carbonsSet CarbonsPlugin {..} localName handler =
  MemoAsync.get carbonsPluginSupported $ \case
    False -> handler $ Left $ featureNotImplemented [i|#{carbonsNS} not supported by the server|]
    True ->
      stanzaRequest
        carbonsPluginSession
        (serverRequest IQSet [closedElement (carbonsName localName)])
        $ \resp -> handler $ case resp of
          Left e -> Left e
          Right _ -> Right ()

-- | Enable message carbons on this session via IQ-set.
carbonsEnable :: (MonadStream m) => CarbonsPlugin m -> (Either StanzaError () -> m ()) -> m ()
carbonsEnable cp = carbonsSet cp "enable"

-- | Disable message carbons on this session via IQ-set.
carbonsDisable :: (MonadStream m) => CarbonsPlugin m -> (Either StanzaError () -> m ()) -> m ()
carbonsDisable cp = carbonsSet cp "disable"

instance (Typeable m) => DiscoInfoProvider (CarbonsPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton carbonsNS

carbonsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
carbonsPlugin pluginsRef = do
  carbonsPluginSlot <- Slot.new
  carbonsPluginIMPlugin <- getIMPlugin pluginsRef
  discoP <- getDiscoPlugin pluginsRef
  carbonsPluginSupported <- newHomeFeatureCheck discoP carbonsNS
  let carbonsPluginSession = pluginsSession pluginsRef
      plugin :: CarbonsPlugin m = CarbonsPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsInHandlers pluginsRef
  addDiscoInfo pluginsRef plugin
