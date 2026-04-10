{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Disco (
  DiscoIdentity (..),
  DiscoFeature,
  DiscoEntity (..),
  emptyDiscoEntity,
  getDiscoEntity,
  DiscoItems,
  getDiscoItems,
  DiscoTopo (..),
  getDiscoTopo,
  DiscoInfo (..),
  emptyDiscoInfo,
  discoInfos,
  discoPlugin,
) where

import Control.Arrow
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Data.RefMap (RefMap)
import qualified Data.RefMap as RefMap
import qualified Data.Registry.Mutable as RegRef
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Session (sessionAddress)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML
import UnliftIO.IORef

data DiscoIdentity = DiscoIdentity
  { discoCategory :: Text
  , discoType :: Text
  }
  deriving (Show, Eq, Ord)

type DiscoFeature = Text

data DiscoEntity = DiscoEntity
  { discoIdentities :: Map DiscoIdentity (Maybe LocalizedText)
  , discoFeatures :: Set DiscoFeature
  }
  deriving (Show, Eq, Generic)

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

discoInfoName :: Text -> Name
discoInfoName = nsName "http://jabber.org/protocol/disco#info"

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
  let discoFeatures = S.fromList features
  when (S.size discoFeatures /= length features) $ Left $ badRequest [i|parseDiscoEntity: non-unique features|]

  return DiscoEntity {..}
 where
  getIdentity e = do
    discoCategory <- requiredAttr "category" e
    discoType <- requiredAttr "type" e
    return DiscoIdentity {..}

  getFeature = requiredAttr "var"

type DiscoCache = Map (XMPPAddress, Maybe DiscoNode) DiscoEntity

newtype DiscoCacheState = DiscoCacheState
  { discoCacheRef :: IORef DiscoCache
  }

-- | Check whether a disco result for the given address should be cached.
shouldCache :: XMPPPluginsRef m -> XMPPAddress -> AllPresencesMap -> Bool
shouldCache pluginsRef addr presences =
  isHomeServer || hasPresence
 where
  myAddress = sessionAddress $ ssSession $ pluginsSession pluginsRef
  isHomeServer = addressLocal addr == Nothing && addressResource addr == Nothing && addressDomain addr == bareDomain (fullBare myAddress)
  hasPresence = case fullJidGet addr of
    Just full -> M.member full presences
    Nothing -> False

-- | Checks cache, then fires an IQ request if needed. Calls handler with the result.
getDiscoEntity :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> (Either StanzaError DiscoEntity -> m ()) -> m ()
getDiscoEntity pluginsRef addr node handler = do
  DiscoCacheState {..} <- RegRef.lookupOrFailM (Proxy :: Proxy DiscoCacheState) $ pluginsHooksSet pluginsRef
  cache <- readIORef discoCacheRef
  let key = (addr, node)
  case M.lookup key cache of
    Just entity -> handler $ Right entity
    Nothing -> do
      let sess = pluginsSession pluginsRef
          req =
            OutRequestIQ
              { oriTo = Just addr
              , oriIqType = IQGet
              , oriChildren = [element (discoInfoName "query") (maybeToList $ fmap ("node",) node) []]
              }
      stanzaRequest sess req $ \ret -> do
        let result = case ret of
              Left e -> Left e
              Right [r] | elementName r == discoInfoName "query" -> parseDiscoEntity r
              _ -> Left $ badRequest "getDiscoEntity: invalid response"
        case result of
          Right entity -> do
            presences <- getAllPresences pluginsRef
            when (shouldCache pluginsRef addr presences) $
              atomicModifyIORef' discoCacheRef $ \m -> (M.insert key entity m, ())
          Left _ -> return ()
        handler result

type DiscoNode = Text
type DiscoItems = Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText)

discoItemsName :: Text -> Name
discoItemsName = nsName "http://jabber.org/protocol/disco#items"

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
      { oriTo = Just addr
      , oriIqType = IQGet
      , oriChildren = [element (discoItemsName "query") (maybeToList $ fmap ("node",) node) []]
      }
    $ \resp -> handler $ case resp of
        Left e -> Left e
        Right [r] -> parseDiscoItems r
        _ -> Left $ badRequest "getDiscoItems: multiple elements in response"

data DiscoTopo = DiscoTopo
  { discoRoot :: DiscoEntity
  , discoItems :: Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText, Either StanzaError DiscoTopo)
  }
  deriving (Show, Eq)

getDiscoTopo :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> Maybe DiscoNode -> (Either StanzaError DiscoTopo -> m ()) -> m ()
getDiscoTopo pluginsRef addr node handler =
  getDiscoEntity pluginsRef addr node $ \case
    Left e -> handler $ Left e
    Right discoRoot ->
      case node of
        Just _ ->
          handler $ Right DiscoTopo {discoItems = M.empty, ..}
        Nothing ->
          getDiscoItems pluginsRef addr node $ \case
            Left e -> handler $ Left e
            Right items ->
              getDiscoTopoItems pluginsRef (M.toList items) [] $ \discoItems ->
                handler $ Right DiscoTopo {discoItems = M.fromList discoItems, ..}

getDiscoTopoItems ::
  (MonadStream m) =>
  XMPPPluginsRef m ->
  [((XMPPAddress, Maybe DiscoNode), Maybe LocalizedText)] ->
  [((XMPPAddress, Maybe DiscoNode), (Maybe LocalizedText, Either StanzaError DiscoTopo))] ->
  ([((XMPPAddress, Maybe DiscoNode), (Maybe LocalizedText, Either StanzaError DiscoTopo))] -> m ()) ->
  m ()
getDiscoTopoItems _ [] acc handler = handler $ reverse acc
getDiscoTopoItems pluginsRef ((k@(сaddr, cnode), name) : rest) acc handler =
  getDiscoTopo pluginsRef сaddr cnode $ \topo ->
    getDiscoTopoItems pluginsRef rest ((k, (name, topo)) : acc) handler

emitNamed :: Map k (Maybe LocalizedText) -> Name -> (k -> [(Name, Text)]) -> [Element]
emitNamed elems name toAttrs = concatMap toNamed $ M.toAscList elems
 where
  toNamed (k, Nothing) = [element name (toAttrs k) []]
  toNamed (k, Just names) = map toOne $ M.toAscList $ localTexts names
   where
    toOne (lang, text) = element name ([("name", text)] ++ xmlLangAttr lang ++ toAttrs k) []

data DiscoInfo = DiscoInfo
  { discoIEntity :: DiscoEntity
  , discoIItems :: DiscoItems
  , discoIChildren :: Map DiscoNode (DiscoEntity, DiscoItems)
  }

emptyDiscoInfo :: DiscoInfo
emptyDiscoInfo =
  DiscoInfo
    { discoIEntity = emptyDiscoEntity
    , discoIItems = M.empty
    , discoIChildren = M.empty
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

data DiscoPlugin m = DiscoPlugin
  { discoPluginInfos :: RefMap (m DiscoInfo)
  , discoPluginCacheState :: DiscoCacheState
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

instance (MonadStream m) => Handler m PresenceUpdate () (DiscoPlugin m) where
  tryHandle (DiscoPlugin {..}) (ResourcePresence full (ResourceUnavailable _)) = do
    let DiscoCacheState {..} = discoPluginCacheState
        addr = fullJidAddress full
    atomicModifyIORef' discoCacheRef $ \m -> (M.filterWithKey (\(a, _) _ -> a /= addr) m, ())
    return Nothing
  tryHandle (DiscoPlugin {..}) (AllResourcesOffline bare _) = do
    let DiscoCacheState {..} = discoPluginCacheState
        bareAddr = bareJidAddress bare
    atomicModifyIORef' discoCacheRef $ \m -> (M.filterWithKey (\(a, _) _ -> addressBare a /= bareAddr) m, ())
    return Nothing
  tryHandle _ _ = return Nothing

discoInfos :: (MonadStream m) => XMPPPluginsRef m -> m (RefMap (m DiscoInfo))
discoInfos = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

discoPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
discoPlugin pluginsRef = do
  discoPluginInfos <- RefMap.new
  discoCacheRef <- newIORef M.empty
  let discoPluginCacheState = DiscoCacheState {..}
      plugin :: DiscoPlugin m = DiscoPlugin {..}
  RegRef.insertNewOrFailM discoPluginInfos $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM discoPluginCacheState $ pluginsHooksSet pluginsRef
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM plugin iqHandlers
  pHandlers <- presenceHandlers pluginsRef
  HL.pushNewOrFailM plugin pHandlers
