{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Disco (
  DiscoIdentity (..),
  DiscoFeature,
  discoInfoNS,
  DiscoEntity (..),
  emptyDiscoEntity,
  DiscoPlugin,
  discoPluginSession,
  discoPluginEntityCacheHandlers,
  discoPluginEntityGetHandlers,
  discoPluginMerged,
  discoPluginMergedSlot,
  getDiscoPlugin,
  DiscoEntityCacheHandlers,
  DiscoEntityGetHandlers,
  DiscoNode,
  getDiscoEntity,
  getDiscoEntityNoCache,
  newDiscoFeatureCheck,
  requestDiscoEntity,
  discoItemsNS,
  DiscoItems,
  requestDiscoItems,
  DiscoTopo (..),
  getDiscoTopo,
  DiscoNodeInfo (..),
  emptyDiscoNodeInfo,
  DiscoInfo (..),
  emptyDiscoInfo,
  DiscoInfoProvider (..),
  featuresDiscoInfo,
  addDiscoInfo,
  getSelfDiscoEntity,
  discoPlugin,
)
where

import Control.Arrow
import Control.AsyncMemo (AsyncMemo)
import qualified Control.AsyncMemo as AsyncMemo
import Control.Concurrent.Linked
import Control.HandlerList (Handler (..), HandlerList)
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Monad.Logger
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import Data.ClassBox (ClassBox (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Registry (Registry)
import qualified Data.Registry as Reg
import Data.Registry.Mutable (RegistryRef)
import qualified Data.Registry.Mutable as RegRef
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import UnliftIO.IORef
import UnliftIO.MVar

data DiscoIdentity = DiscoIdentity
  { discoCategory :: Text
  , discoType :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON DiscoIdentity
instance FromJSON DiscoIdentity

type DiscoFeature = Text

data DiscoEntity = DiscoEntity
  { discoIdentities :: Map DiscoIdentity (Maybe LocalizedText)
  , discoFeatures :: Set DiscoFeature
  }
  deriving (Show, Eq, Generic)

-- | JSON encoding: @{ identities: [[ident, names], ...], features: [feat, ...] }@.
instance ToJSON DiscoEntity where
  toJSON DiscoEntity {..} =
    JSON.object
      [ "identities" .= M.toList discoIdentities
      , "features" .= discoFeatures
      ]

instance FromJSON DiscoEntity where
  parseJSON = JSON.withObject "DiscoEntity" $ \o -> do
    ids <- o .: "identities"
    feats <- o .: "features"
    return
      DiscoEntity
        { discoIdentities = M.fromList ids
        , discoFeatures = feats
        }

emptyDiscoEntity :: DiscoEntity
emptyDiscoEntity =
  DiscoEntity
    { discoIdentities = M.empty
    , discoFeatures = S.empty
    }

discoEntityUnion :: DiscoEntity -> DiscoEntity -> Maybe DiscoEntity
discoEntityUnion a b = do
  discoIdentities <- mapDisjointUnion (discoIdentities a) (discoIdentities b)
  discoFeatures <- setDisjointUnion (discoFeatures a) (discoFeatures b)
  return DiscoEntity {..}

discoInfoNS :: Text
discoInfoName :: Text -> Name
(discoInfoNS, discoInfoName) = namePair "http://jabber.org/protocol/disco#info"

requiredAttr :: Name -> Element -> Either StanzaError Text
requiredAttr name e = case getAttr name e of
  Nothing -> Left $ badRequest [i|no #{name} attribute in #{elementName e}|]
  Just r -> return r

parseNamed :: (Show k, Ord k) => Element -> Name -> (Element -> Either StanzaError k) -> Either StanzaError (Map k (Maybe LocalizedText))
parseNamed root name getKey = do
  items <- mapM getElem $ fromElement root $/ XC.element name &| curElement
  namedMap <- mapM sequence $ M.fromListWithKey (M.unionWith . conflict) $ fmap (second $ fmap return) items
  return $ fmap localizedFromTexts namedMap
 where
  getElem e = do
    k <- getKey e
    let names = case getAttr "name" e of
          Nothing -> M.empty
          Just text -> M.singleton (xmlLangGet e) text
    return (k, names)
  conflict feature _ _ = Left $ badRequest [i|conflicting feature #{show feature}|]

parseDiscoEntity :: Element -> Either StanzaError DiscoEntity
parseDiscoEntity re = do
  discoIdentities <- parseNamed re (discoInfoName "identity") getIdentity
  features <- mapM getFeature $ fromElement re $/ XC.element (discoInfoName "feature") &| curElement
  let discoFeatures' = S.fromList features
  when (S.size discoFeatures' /= length features) $ Left $ badRequest [i|non-unique features|]
  -- XEP-0030: expect disco#info to be supported even if not advertised.
  let discoFeatures = S.insert discoInfoNS discoFeatures'

  return DiscoEntity {..}
 where
  getIdentity e = do
    discoCategory <- requiredAttr "category" e
    discoType <- requiredAttr "type" e
    return DiscoIdentity {..}

  getFeature = requiredAttr "var"

{- | Handler list for resolving disco entity requests from cache. If a handler
matches the request, it must fire the callback itself and return @Just ()@;
otherwise return @Nothing@ to fall through.
-}
type DiscoEntityCacheHandlers m = HandlerList m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) ()

{- | Handler list for resolving disco entity requests from a non-cache source
(e.g. an external storage-backed lookup like XEP-0115 caps). Sits between the
in-process cache layer in 'getDiscoEntity' and the direct IQ in
'requestDiscoEntity'; if a handler claims the request it must fire the
callback itself and return @Just ()@.
-}
type DiscoEntityGetHandlers m = HandlerList m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) ()

-- | Direct disco#info IQ fetch — bypasses the cache handler chain.
requestDiscoEntity :: (MonadStream m, ToXMPPAddress addr) => StanzaSession m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
requestDiscoEntity sess addr node handler = do
  let req =
        OutRequestIQ
          { oriTo = Just (toXMPPAddress addr)
          , oriIqType = IQGet
          , oriChildren = [element (discoInfoName "query") (maybeToList $ fmap ("node",) node) []]
          }
  stanzaRequest sess req $ \ret ->
    handler $ case ret of
      Left e -> Left e
      Right [r] | elementName r == discoInfoName "query" -> parseDiscoEntity r
      _ -> Left $ badRequest "invalid disco#info response"

-- | Direct disco#items IQ fetch — bypasses the cache handler chain.
requestDiscoItems :: (MonadStream m, ToXMPPAddress addr) => StanzaSession m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoItems -> m ()) -> m ()
requestDiscoItems sess addr node handler =
  stanzaRequest
    sess
    OutRequestIQ
      { oriTo = Just (toXMPPAddress addr)
      , oriIqType = IQGet
      , oriChildren = [element (discoItemsName "query") (maybeToList $ fmap ("node",) node) []]
      }
    $ \resp ->
      handler $ case resp of
        Left e -> Left e
        Right [r] -> parseDiscoItems r
        _ -> Left $ badRequest "multiple elements in disco#items response"

{- | Get disco#info, first consulting the entity cache handlers, then the
get-handlers, then a direct IQ.
-}
getDiscoEntity :: (MonadStream m, ToXMPPAddress addr) => DiscoPlugin m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getDiscoEntity dp@(DiscoPlugin {..}) addr0 node handler = do
  let addr = toXMPPAddress addr0
  handled <- HL.call discoPluginEntityCacheHandlers (addr, node, handler)
  case handled of
    Just () -> return ()
    Nothing -> getDiscoEntityNoCache dp addr node handler

{- | Get disco#info skipping the in-process cache layer: first consult the
entity get-handlers, then fall back to a direct IQ.
-}
getDiscoEntityNoCache :: (MonadStream m, ToXMPPAddress addr) => DiscoPlugin m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getDiscoEntityNoCache (DiscoPlugin {..}) addr0 node handler = do
  let addr = toXMPPAddress addr0
  handled <- HL.call discoPluginEntityGetHandlers (addr, node, handler)
  case handled of
    Just () -> return ()
    Nothing -> requestDiscoEntity discoPluginSession addr node handler

{- | Build a memoized check of whether an entity advertises a disco feature.
The underlying entity fetch can be memoized by a cache handler (e.g. the
home-server cache); wrapping the boolean result means each consumer can cache
its own answer instead of re-traversing the feature set. Disco errors collapse
to 'False' (treat the feature as unsupported).
-}
newDiscoFeatureCheck :: (MonadStream m, ToXMPPAddress addr) => DiscoPlugin m -> addr -> Maybe DiscoNode -> DiscoFeature -> m (AsyncMemo m Bool)
newDiscoFeatureCheck dp addr node feat = AsyncMemo.new $ \cb ->
  getDiscoEntity dp addr node $ \case
    Left _ -> cb False
    Right ent -> cb (feat `S.member` discoFeatures ent)

{- | Request disco#info for the local account by sending an IQ with no @to@
attribute (RFC 6120 §10.3.3: handled by the server on behalf of the account).
Used where the target is the session's own bare JID — e.g. XEP-0313 requires
MAM support to be advertised at the account's own bare JID, not at the server
domain. Does not consult the cache-handler chain.
-}
getSelfDiscoEntity :: (MonadStream m) => DiscoPlugin m -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getSelfDiscoEntity (DiscoPlugin {discoPluginSession}) handler =
  stanzaRequest discoPluginSession req $ \ret -> handler $ case ret of
    Left e -> Left e
    Right [r] | elementName r == discoInfoName "query" -> parseDiscoEntity r
    _ -> Left $ badRequest "invalid disco#info response"
 where
  req = serverRequest IQGet [closedElement (discoInfoName "query")]

type DiscoNode = Text

type DiscoItems = Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText)

discoItemsNS :: Text
discoItemsName :: Text -> Name
(discoItemsNS, discoItemsName) = namePair "http://jabber.org/protocol/disco#items"

parseDiscoItems :: Element -> Either StanzaError DiscoItems
parseDiscoItems re = parseNamed re (discoItemsName "item") getItem
 where
  getItem e = do
    address' <- requiredAttr "jid" e
    address <- case xmppAddress address' of
      Left err -> Left $ jidMalformed [i|malformed jid #{address'}: #{err}|]
      Right r -> return r
    let node = getAttr "node" e
    return (address, node)

data DiscoTopo = DiscoTopo
  { discoRoot :: DiscoEntity
  , discoItems :: Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText, Either StanzaError DiscoTopo)
  }
  deriving (Show, Eq)

getDiscoTopo :: (MonadStream m, ToXMPPAddress addr) => DiscoPlugin m -> addr -> Maybe DiscoNode -> (Either StanzaError DiscoTopo -> m ()) -> m ()
getDiscoTopo dp@(DiscoPlugin {discoPluginSession}) addr0 node handler = do
  let addr = toXMPPAddress addr0
  $(logDebug) [i|getDiscoTopo: starting for #{addressToText addr} node=#{show node}|]
  getDiscoEntity dp addr node $ \case
    Left e -> do
      $(logDebug) [i|getDiscoTopo: entity error for #{addressToText addr}: #{e}|]
      handler $ Left e
    Right discoRoot | discoItemsNS `S.member` discoFeatures discoRoot ->
      requestDiscoItems discoPluginSession addr node $ \case
        Left e -> do
          $(logDebug) [i|getDiscoTopo: items error for #{addressToText addr}: #{e}|]
          handler $ Left e
        Right items ->
          getDiscoTopoItems dp (M.toList items) $ \discoItems ->
            handler $ Right DiscoTopo {..}
    Right discoRoot -> handler $ Right DiscoTopo {discoItems = M.empty, ..}

getDiscoTopoItems ::
  (MonadStream m) =>
  DiscoPlugin m ->
  [((XMPPAddress, Maybe DiscoNode), Maybe LocalizedText)] ->
  (Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText, Either StanzaError DiscoTopo) -> m ()) ->
  m ()
getDiscoTopoItems _ [] handler = handler M.empty
getDiscoTopoItems dp items handler = void $ forkLinked $ do
  vars <- forM items $ \(k@(сaddr, cnode), name) -> do
    resultVar <- newEmptyMVar
    getDiscoTopo dp сaddr cnode $ putMVar resultVar
    return (k, name, resultVar)
  results <- forM vars $ \(k, name, resultVar) -> do
    topo <- takeMVar resultVar
    return (k, (name, topo))
  handler $ M.fromList results

emitNamed :: Map k (Maybe LocalizedText) -> Name -> (k -> [(Name, Text)]) -> [Element]
emitNamed elems name toAttrs = concatMap toNamed $ M.toAscList elems
 where
  toNamed (k, Nothing) = [element name (toAttrs k) []]
  toNamed (k, Just names) = map toOne $ M.toAscList $ localTexts names
   where
    toOne (lang, text) = element name ([("name", text)] ++ xmlLangAttr lang ++ toAttrs k) []

data DiscoNodeInfo = DiscoNodeInfo
  { discoNEntity :: DiscoEntity
  , discoNItems :: Maybe DiscoItems
  }

emptyDiscoNodeInfo :: DiscoNodeInfo
emptyDiscoNodeInfo =
  DiscoNodeInfo
    { discoNEntity = emptyDiscoEntity
    , discoNItems = Nothing
    }

discoNodeInfoUnion :: DiscoNodeInfo -> DiscoNodeInfo -> Maybe DiscoNodeInfo
discoNodeInfoUnion a b = do
  discoNEntity <- discoEntityUnion (discoNEntity a) (discoNEntity b)
  discoNItems <- case (discoNItems a, discoNItems b) of
    (Nothing, x) -> Just x
    (x, Nothing) -> Just x
    (Just ia, Just ib) -> Just <$> mapDisjointUnion ia ib
  return DiscoNodeInfo {..}

data DiscoInfo = DiscoInfo
  { discoINode :: DiscoNodeInfo
  , discoIChildren :: Map DiscoNode DiscoNodeInfo
  }

emptyDiscoInfo :: DiscoInfo
emptyDiscoInfo =
  DiscoInfo
    { discoINode = emptyDiscoNodeInfo
    , discoIChildren = M.empty
    }

discoInfoUnion :: DiscoInfo -> DiscoInfo -> Maybe DiscoInfo
discoInfoUnion a b = do
  discoINode <- discoNodeInfoUnion (discoINode a) (discoINode b)
  discoIChildren <- mapDisjointUnion (discoIChildren a) (discoIChildren b)
  return DiscoInfo {..}

emitDiscoEntity :: DiscoEntity -> [Element]
emitDiscoEntity (DiscoEntity {..}) = identities ++ features
 where
  makeIdentityAttr (DiscoIdentity {..}) = [("category", discoCategory), ("type", discoType)]
  identities = emitNamed discoIdentities (discoInfoName "identity") makeIdentityAttr
  features = map (\f -> element (discoInfoName "feature") [("var", f)] []) $ S.toAscList discoFeatures

emitDiscoItems :: DiscoItems -> [Element]
emitDiscoItems items = emitNamed items (discoItemsName "item") makeItemAttr
 where
  makeItemAttr (addr, mNode) = ("jid", addressToText addr) : maybeToList (fmap ("node",) mNode)

-- | Class for disco info providers. Each provider type can only be registered once.
class (Typeable a) => DiscoInfoProvider a where
  discoProviderInfo :: a -> DiscoInfo

data DiscoPlugin m = DiscoPlugin
  { discoPluginSession :: StanzaSession m
  , discoPluginProviders :: RegistryRef DiscoInfoProvider
  , discoPluginMerged :: IORef DiscoInfo
  , discoPluginEntityCacheHandlers :: DiscoEntityCacheHandlers m
  , discoPluginEntityGetHandlers :: DiscoEntityGetHandlers m
  {- ^ Fired with the freshly merged 'DiscoInfo' after every successful
  'addDiscoInfo' call. Subscribe to recompute derived data (e.g.
  XEP-0115/0390 verification hashes).
  -}
  , discoPluginMergedSlot :: Slot m DiscoInfo
  }

-- | Add disco#items feature to a node info if it has items.
addItemsFeature :: DiscoNodeInfo -> Maybe DiscoNodeInfo
addItemsFeature node = case discoNItems node of
  Nothing -> Just node
  Just _ -> discoNodeInfoUnion node $ featuresDiscoNodeInfo $ S.singleton discoItemsNS

mergeProviders :: Registry DiscoInfoProvider -> Maybe DiscoInfo
mergeProviders reg = do
  let infos = map (\(ClassBox p) -> discoProviderInfo p) $ Reg.toList reg
  r <- foldr (\a acc -> acc >>= discoInfoUnion a) (Just emptyDiscoInfo) infos
  rootNode <- addItemsFeature $ discoINode r
  children <- mapM addItemsFeature $ discoIChildren r
  return r {discoINode = rootNode, discoIChildren = children}

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (DiscoPlugin m) where
  tryHandle (DiscoPlugin {discoPluginMerged}) (InRequestIQ {iriType = IQGet, iriChildren = [req]}) = do
    DiscoInfo {discoINode = DiscoNodeInfo {..}, ..} <- readIORef discoPluginMerged
    let res
          | elementName req == infoName = Just $ fmap (infoName,) $ case getAttr "node" req of
              Nothing -> Just $ emitDiscoEntity discoNEntity
              Just node | Just DiscoNodeInfo {discoNEntity = entity} <- M.lookup node discoIChildren -> Just $ emitDiscoEntity entity
              _ -> Nothing
          | elementName req == itemsName = Just $ fmap (itemsName,) $ case getAttr "node" req of
              Nothing -> fmap emitDiscoItems discoNItems
              Just node | Just DiscoNodeInfo {discoNItems = Just items} <- M.lookup node discoIChildren -> Just $ emitDiscoItems items
              _ -> Nothing
          | otherwise = Nothing
    case res of
      Nothing -> return Nothing
      Just Nothing -> return $ Just $ IQError $ itemNotFound "Unknown node"
      Just (Just (name, elems)) ->
        return $ Just $ IQResult [element name (maybeToList $ ("node",) <$> getAttr "node" req) $ map NodeElement elems]
   where
    infoName = discoInfoName "query"
    itemsName = discoItemsName "query"
  tryHandle _ _ = return Nothing

addDiscoInfo :: forall m a. (MonadStream m, DiscoInfoProvider a) => XMPPPluginsRef m -> a -> m ()
addDiscoInfo pluginsRef provider = do
  dp <- getDiscoPlugin pluginsRef
  RegRef.insertNewOrFailM provider $ discoPluginProviders dp
  reg <- RegRef.read $ discoPluginProviders dp
  result <- atomicModifyIORef (discoPluginMerged dp) $ \old ->
    case mergeProviders reg of
      Nothing -> (old, Nothing)
      Just merged -> (merged, Just merged)
  case result of
    Nothing -> do
      RegRef.delete (Proxy :: Proxy a) $ discoPluginProviders dp
      fail "addDiscoInfo: overlapping disco infos"
    Just merged -> Slot.call (discoPluginMergedSlot dp) merged

getDiscoPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (DiscoPlugin m)
getDiscoPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (DiscoPlugin m)) $ pluginsHooksSet pluginsRef

featuresDiscoNodeInfo :: Set DiscoFeature -> DiscoNodeInfo
featuresDiscoNodeInfo features = emptyDiscoNodeInfo {discoNEntity = emptyDiscoEntity {discoFeatures = features}}

featuresDiscoInfo :: Maybe DiscoNode -> Set DiscoFeature -> DiscoInfo
featuresDiscoInfo Nothing features = emptyDiscoInfo {discoINode = featuresDiscoNodeInfo features}
featuresDiscoInfo (Just node) features = emptyDiscoInfo {discoIChildren = M.singleton node $ featuresDiscoNodeInfo features}

{- | The 'DiscoPlugin' is itself a disco info provider (contributes the
baseline @disco#info@ feature).
-}
instance (Typeable m) => DiscoInfoProvider (DiscoPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton discoInfoNS

discoPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
discoPlugin pluginsRef = do
  discoPluginProviders <- RegRef.new
  discoPluginMerged <- newIORef emptyDiscoInfo
  discoPluginEntityCacheHandlers <- HL.new :: m (DiscoEntityCacheHandlers m)
  discoPluginEntityGetHandlers <- HL.new :: m (DiscoEntityGetHandlers m)
  discoPluginMergedSlot <- Slot.new
  let discoPluginSession = pluginsSession pluginsRef
      plugin :: DiscoPlugin m = DiscoPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsIQHandlers pluginsRef
  addDiscoInfo pluginsRef plugin
