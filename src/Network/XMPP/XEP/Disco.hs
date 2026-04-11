{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Disco
  ( DiscoIdentity (..),
    DiscoFeature,
    discoInfoNS,
    DiscoEntity (..),
    emptyDiscoEntity,
    getDiscoEntity,
    discoItemsNS,
    DiscoItems,
    getDiscoItems,
    DiscoTopo (..),
    getDiscoTopo,
    DiscoInfo (..),
    emptyDiscoInfo,
    discoInfos,
    discoEntityCacheHandlers,
    discoPlugin,
  )
where

import Control.Arrow
import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.Concurrent.Linked
import Control.HandlerList (Handler (..), HandlerList)
import qualified Control.HandlerList as HL
import Control.LazyOnce (LazyOnce)
import qualified Control.LazyOnce as LazyOnce
import Control.Monad
import Control.Monad.Logger
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.RefMap (RefMap)
import qualified Data.RefMap as RefMap
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Session (sessionAddress)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import UnliftIO.MVar

data DiscoIdentity = DiscoIdentity
  { discoCategory :: Text,
    discoType :: Text
  }
  deriving (Show, Eq, Ord)

type DiscoFeature = Text

data DiscoEntity = DiscoEntity
  { discoIdentities :: Map DiscoIdentity (Maybe LocalizedText),
    discoFeatures :: Set DiscoFeature
  }
  deriving (Show, Eq, Generic)

emptyDiscoEntity :: DiscoEntity
emptyDiscoEntity =
  DiscoEntity
    { discoIdentities = M.empty,
      discoFeatures = S.empty
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
  Nothing -> Left $ badRequest [i|requiredAttr: no $name attribute in #{elementName e}|]
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
    conflict feature _ _ = Left $ badRequest [i|parseNamed: conflicting feature #{show feature}|]

parseDiscoEntity :: Element -> Either StanzaError DiscoEntity
parseDiscoEntity re = do
  discoIdentities <- parseNamed re (discoInfoName "identity") getIdentity
  features <- mapM getFeature $ fromElement re $/ XC.element (discoInfoName "feature") &| curElement
  let discoFeatures' = S.fromList features
  when (S.size discoFeatures' /= length features) $ Left $ badRequest [i|parseDiscoEntity: non-unique features|]
  -- XEP-0030: expect disco#info to be supported even if not advertised.
  let discoFeatures = S.insert discoInfoNS discoFeatures'

  return DiscoEntity {..}
  where
    getIdentity e = do
      discoCategory <- requiredAttr "category" e
      discoType <- requiredAttr "type" e
      return DiscoIdentity {..}

    getFeature = requiredAttr "var"

-- | Lazy disco entity cache stored in each presence's extended registry.
newtype LazyDiscoEntity m = LazyDiscoEntity (LazyOnce m (Either StanzaError DiscoEntity))

instance Show (LazyDiscoEntity m) where
  show _ = "LazyDiscoEntity"

-- | Homeserver disco entity cache.
newtype DiscoHomeCache m = DiscoHomeCache (LazyOnce m (Either StanzaError DiscoEntity))

-- | Handler list for resolving disco entity requests from cache (e.g. Caps).
type DiscoEntityCacheHandlers m = HandlerList m (XMPPAddress, Maybe DiscoNode) (Either StanzaError DiscoEntity)

-- | Get the disco entity cache handler list from the plugins hook set.
discoEntityCacheHandlers :: (MonadStream m) => XMPPPluginsRef m -> m (DiscoEntityCacheHandlers m)
discoEntityCacheHandlers pluginsRef = RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

-- | Codec that creates a LazyDiscoEntity on decode (using the FullJID
-- from meta) and strips it on encode.
data DiscoEntityCodec m = DiscoEntityCodec (XMPPPluginsRef m)

instance (MonadStream m) => Codec m FullJID Presence (DiscoEntityCodec m) where
  codecDecode (DiscoEntityCodec pluginsRef) faddr pres = do
    let addr = fullJidAddress faddr
    lazy <- LazyOnce.new $ doGetDiscoEntitySync pluginsRef addr Nothing
    let lde = LazyDiscoEntity lazy :: LazyDiscoEntity m
    return $ pres {presenceExtended = Reg.insert lde (presenceExtended pres)}
  codecEncode _ _ pres =
    return $ pres {presenceExtended = Reg.delete (Proxy :: Proxy (LazyDiscoEntity m)) (presenceExtended pres)}

-- | Perform a disco#info request, first checking cache handlers (e.g. Caps),
-- then falling back to an IQ request.
doGetDiscoEntity :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
doGetDiscoEntity pluginsRef addr node handler = do
  cacheHandlers <- discoEntityCacheHandlers pluginsRef
  cached <- HL.call cacheHandlers (addr, node)
  case cached of
    Just result -> handler result
    Nothing -> do
      let sess = pluginsSession pluginsRef
          req =
            OutRequestIQ
              { oriTo = Just addr,
                oriIqType = IQGet,
                oriChildren = [element (discoInfoName "query") (maybeToList $ fmap ("node",) node) []]
              }
      stanzaRequest sess req $ \ret -> do
        let result = case ret of
              Left e -> Left e
              Right [r] | elementName r == discoInfoName "query" -> parseDiscoEntity r
              _ -> Left $ badRequest "getDiscoEntity: invalid response"
        handler result

-- | Synchronous wrapper around doGetDiscoEntity, for use inside LazyOnce.
doGetDiscoEntitySync :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> m (Either StanzaError DiscoEntity)
doGetDiscoEntitySync pluginsRef addr node = do
  resultVar <- newEmptyMVar
  doGetDiscoEntity pluginsRef addr node $ putMVar resultVar
  takeMVar resultVar

-- | Get disco entity info, using cached LazyOnce values for JIDs with
-- active presences and for the homeserver.
getDiscoEntity :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getDiscoEntity pluginsRef addr node handler
  | isHomeServer,
    Nothing <- node = do
      DiscoHomeCache lazy <- RegRef.lookupOrFailM (Proxy :: Proxy (DiscoHomeCache m)) $ pluginsHooksSet pluginsRef
      LazyOnce.get lazy >>= handler
  | Just full <- fullJidGet addr,
    Nothing <- node = do
      presences <- getAllPresences pluginsRef
      case M.lookup full presences of
        Just pres
          | Just (LazyDiscoEntity lazy) <- Reg.lookup (Proxy :: Proxy (LazyDiscoEntity m)) (presenceExtended pres) ->
              LazyOnce.get lazy >>= handler
        _ -> doGetDiscoEntity pluginsRef addr node handler
  | otherwise = doGetDiscoEntity pluginsRef addr node handler
  where
    myAddress = sessionAddress $ ssSession $ pluginsSession pluginsRef
    isHomeServer = addressLocal addr == Nothing && addressResource addr == Nothing && addressDomain addr == bareDomain (fullBare myAddress)

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
        Left err -> Left $ jidMalformed [i|parseDiscoItems: malformed jid #{address'}: #{err}|]
        Right r -> return r
      let node = getAttr "node" e
      return (address, node)

getDiscoItems :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> (Either StanzaError DiscoItems -> m ()) -> m ()
getDiscoItems pluginsRef addr node handler = do
  let sess = pluginsSession pluginsRef
  stanzaRequest
    sess
    OutRequestIQ
      { oriTo = Just addr,
        oriIqType = IQGet,
        oriChildren = [element (discoItemsName "query") (maybeToList $ fmap ("node",) node) []]
      }
    $ \resp -> handler $ case resp of
      Left e -> Left e
      Right [r] -> parseDiscoItems r
      _ -> Left $ badRequest "getDiscoItems: multiple elements in response"

data DiscoTopo = DiscoTopo
  { discoRoot :: DiscoEntity,
    discoItems :: Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText, Either StanzaError DiscoTopo)
  }
  deriving (Show, Eq)

getDiscoTopo :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> (Either StanzaError DiscoTopo -> m ()) -> m ()
getDiscoTopo pluginsRef addr node handler = do
  $(logDebug) [i|getDiscoTopo: starting for #{addressToText addr} node=#{show node}|]
  getDiscoEntity pluginsRef addr node $ \case
    Left e -> do
      $(logDebug) [i|getDiscoTopo: entity error for #{addressToText addr}: #{e}|]
      handler $ Left e
    Right discoRoot | discoItemsNS `S.member` discoFeatures discoRoot ->
      getDiscoItems pluginsRef addr node $ \case
        Left e -> do
          $(logDebug) [i|getDiscoTopo: items error for #{addressToText addr}: #{e}|]
          handler $ Left e
        Right items ->
          getDiscoTopoItems pluginsRef (M.toList items) $ \discoItems ->
            handler $ Right DiscoTopo {..}
    Right discoRoot -> handler $ Right DiscoTopo {discoItems = M.empty, ..}

getDiscoTopoItems ::
  (MonadStream m) =>
  XMPPPluginsRef m ->
  [((XMPPAddress, Maybe DiscoNode), Maybe LocalizedText)] ->
  (Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText, Either StanzaError DiscoTopo) -> m ()) ->
  m ()
getDiscoTopoItems _ [] handler = handler M.empty
getDiscoTopoItems pluginsRef items handler = void $ forkLinked $ do
  vars <- forM items $ \(k@(сaddr, cnode), name) -> do
    resultVar <- newEmptyMVar
    getDiscoTopo pluginsRef сaddr cnode $ putMVar resultVar
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

data DiscoInfo = DiscoInfo
  { discoIEntity :: DiscoEntity,
    discoIItems :: DiscoItems,
    discoIChildren :: Map DiscoNode (DiscoEntity, DiscoItems)
  }

emptyDiscoInfo :: DiscoInfo
emptyDiscoInfo =
  DiscoInfo
    { discoIEntity = emptyDiscoEntity,
      discoIItems = M.empty,
      discoIChildren = M.empty
    }

discoInfoUnion :: DiscoInfo -> DiscoInfo -> Maybe DiscoInfo
discoInfoUnion a b = do
  discoIEntity <- discoEntityUnion (discoIEntity a) (discoIEntity b)
  discoIItems <- mapDisjointUnion (discoIItems a) (discoIItems b)
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

newtype DiscoPlugin m = DiscoPlugin
  { discoPluginInfos :: RefMap (m DiscoInfo)
  }

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (DiscoPlugin m) where
  tryHandle (DiscoPlugin {..}) (InRequestIQ {iriType = IQGet, iriChildren = [req]}) = do
    infos <- RefMap.entries discoPluginInfos >>= sequence
    DiscoInfo {..} <- case foldr (\a acc -> acc >>= discoInfoUnion a) (Just emptyDiscoInfo) infos of
      Nothing -> fail "discoIqHandler: overlapping plugins"
      Just r -> return r
    let res
          | elementName req == infoName = Just $ fmap (infoName,) $ case getAttr "node" req of
              Nothing -> Just $ emitDiscoEntity discoIEntity
              Just node | Just (entity, _) <- M.lookup node discoIChildren -> Just $ emitDiscoEntity entity
              _ -> Nothing
          | elementName req == itemsName = Just $ fmap (itemsName,) $ case getAttr "node" req of
              Nothing -> Just $ emitDiscoItems discoIItems
              Just node | Just (_, items) <- M.lookup node discoIChildren -> Just $ emitDiscoItems items
              _ -> Nothing
          | otherwise = Nothing
    case res of
      Nothing -> return Nothing
      Just Nothing -> return $ Just $ IQError $ itemNotFound "discoIqHandler: unknown node"
      Just (Just (name, elems)) ->
        return $ Just $ IQResult [element name (maybeToList $ ("node",) <$> getAttr "node" req) $ map NodeElement elems]
    where
      infoName = discoInfoName "query"
      itemsName = discoItemsName "query"
  tryHandle _ _ = return Nothing

discoInfos :: (MonadStream m) => XMPPPluginsRef m -> m (RefMap (m DiscoInfo))
discoInfos = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

discoPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
discoPlugin pluginsRef = do
  discoPluginInfos <- RefMap.new
  discoCacheHandlers <- HL.new :: m (DiscoEntityCacheHandlers m)
  let myAddress = sessionAddress $ ssSession $ pluginsSession pluginsRef
      homeAddr = XMPPAddress Nothing (bareDomain $ fullBare myAddress) Nothing
  homeLazy <- LazyOnce.new $ doGetDiscoEntitySync pluginsRef homeAddr Nothing
  let plugin :: DiscoPlugin m = DiscoPlugin {..}
  RegRef.insertNewOrFailM discoPluginInfos $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM discoCacheHandlers $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM (DiscoHomeCache homeLazy :: DiscoHomeCache m) $ pluginsHooksSet pluginsRef
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM plugin iqHandlers
  codecs <- presenceCodecs pluginsRef
  Codec.pushNewOrFailM (DiscoEntityCodec pluginsRef :: DiscoEntityCodec m) codecs
