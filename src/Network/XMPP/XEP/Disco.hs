module Network.XMPP.XEP.Disco
  ( DiscoIdentity(..)
  , DiscoFeature
  , DiscoEntity(..)
  , emptyDiscoEntity
  , getDiscoEntity
  , DiscoItems
  , getDiscoItems
  , DiscoTopo(..)
  , getDiscoTopo
  , DiscoPlugin(..)
  , emptyDiscoPlugin
  , discoPlugin
  ) where

import Control.Arrow
import Control.Monad
import Data.Maybe
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.String.Interpolate (i)

import Network.XMPP.Utils
import Network.XMPP.XML
import Network.XMPP.Plugin
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.Language

data DiscoIdentity = DiscoIdentity { discoCategory :: Text
                                   , discoType :: Text
                                   }
                   deriving (Show, Eq, Ord)

type DiscoFeature = Text

data DiscoEntity = DiscoEntity { discoIdentities :: Map DiscoIdentity (Maybe LocalizedText)
                               , discoFeatures :: Set DiscoFeature
                               }
                 deriving (Show, Eq, Generic)

emptyDiscoEntity :: DiscoEntity
emptyDiscoEntity = DiscoEntity { discoIdentities = M.empty
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
  namedMap <- sequence $ fmap sequence $ M.fromListWithKey (\k -> M.unionWith $ conflict k) $ fmap (second $ fmap return) items
  return $ fmap localizedFromTexts namedMap
  
  where getElem e = do
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
  where getIdentity e = do
          discoCategory <- requiredAttr "category" e
          discoType <- requiredAttr "type" e
          return DiscoIdentity {..}

        getFeature e = requiredAttr "var" e

getDiscoEntity :: MonadStream m => StanzaSession m -> XMPPAddress -> Maybe DiscoNode -> m (Either StanzaError DiscoEntity)
getDiscoEntity sess addr node = do
  ret <- stanzaSyncRequest sess OutRequestIQ { oriTo = Just addr
                                            , oriIqType = IQGet
                                            , oriChildren = [element (discoInfoName "query") (maybeToList $ fmap ("node", ) node) []]
                                            }
  return $ case ret of
    Left (e, _) -> Left e
    Right [r] | elementName r == discoInfoName "query" -> parseDiscoEntity r
    _ -> Left $ badRequest "getDiscoEntity: invalid response"

type DiscoNode = Text
type DiscoItems = Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText)

discoItemsName :: Text -> Name
discoItemsName = nsName "http://jabber.org/protocol/disco#items"

parseDiscoItems :: Element -> Either StanzaError DiscoItems
parseDiscoItems re = parseNamed re (discoItemsName "item") getItem
  where getItem e = do
          address' <- requiredAttr "jid" e
          address <- case xmppAddress address' of
            Left err -> Left $ jidMalformed [i|parseDiscoItems: malformed jid #{address'}: #{err}|]
            Right r -> return r
          let node = getAttr "node" e
          return (address, node)

getDiscoItems :: MonadStream m => StanzaSession m -> XMPPAddress -> Maybe DiscoNode -> m (Either StanzaError DiscoItems)
getDiscoItems sess addr node = do
  ret <- stanzaSyncRequest sess OutRequestIQ { oriTo = Just addr
                                            , oriIqType = IQGet
                                            , oriChildren = [element (discoItemsName "query") (maybeToList $ fmap ("node", ) node) []]
                                            }
  return $ case ret of
    Left (e, _) -> Left e
    Right [r] -> parseDiscoItems r
    _ -> Left $ badRequest "getDiscoItems: multiple elements in response"

data DiscoTopo = DiscoTopo { discoRoot :: DiscoEntity
                           , discoItems :: Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText, Either StanzaError DiscoTopo)
                           }
               deriving (Show, Eq)

getDiscoTopo :: MonadStream m => StanzaSession m -> XMPPAddress -> Maybe DiscoNode -> m (Either StanzaError DiscoTopo)
getDiscoTopo sess addr node = runExceptT $ do
  discoRoot <- ExceptT $ getDiscoEntity sess addr node
  case node of
    Nothing -> do
      items <- ExceptT $ getDiscoItems sess addr node
      discoItems <- fmap M.fromList $ forM (M.toList items) $ \(k@(сaddr, cnode), name) -> do
        topo <- lift $ getDiscoTopo sess сaddr cnode
        return (k, (name, topo))
      return DiscoTopo {..}
      
    Just _ -> return DiscoTopo { discoItems = M.empty
                              , ..
                              }


emitNamed :: Map k (Maybe LocalizedText) -> Name -> (k -> [(Name, Text)]) -> [Element]
emitNamed elems name toAttrs = concatMap toNamed $ M.toAscList elems
  where toNamed (k, Nothing) = [element name (toAttrs k) []]
        toNamed (k, Just names) = map toOne $ M.toAscList $ localTexts names
          where toOne (lang, text) = element name ([("name", text)] ++ xmlLangAttr lang ++ toAttrs k) []

data DiscoPlugin = DiscoPlugin { discoPEntity :: DiscoEntity
                               , discoPItems :: DiscoItems
                               , discoPChildren :: Map DiscoNode (DiscoEntity, DiscoItems)
                               }

emptyDiscoPlugin :: DiscoPlugin
emptyDiscoPlugin = DiscoPlugin { discoPEntity = emptyDiscoEntity
                               , discoPItems = M.empty
                               , discoPChildren = M.empty
                               }

discoPluginUnion :: DiscoPlugin -> DiscoPlugin -> Maybe DiscoPlugin
discoPluginUnion a b = do
  discoPEntity <- discoEntityUnion (discoPEntity a) (discoPEntity b)
  discoPItems <- mapDisjointUnion (discoPItems a) (discoPItems b)
  discoPChildren <- mapDisjointUnion (discoPChildren a) (discoPChildren b)
  return DiscoPlugin {..}

emitDiscoEntity :: DiscoEntity -> [Element]
emitDiscoEntity (DiscoEntity {..}) = identities ++ features
  where makeIdentityAttr (DiscoIdentity {..}) = [("category", discoCategory), ("type", discoType)]
        identities = emitNamed discoIdentities (discoInfoName "identity") makeIdentityAttr
        features = map (\f -> element (discoInfoName "feature") [("var", f)] []) $ S.toAscList discoFeatures

emitDiscoItems :: DiscoItems -> [Element]
emitDiscoItems items = emitNamed items (discoItemsName "item") makeItemAttr
  where makeItemAttr (addr, mNode) = [("jid", addressToText addr)] ++ maybeToList (fmap ("node", ) mNode)

discoIqHandler :: MonadStream m => DiscoPlugin -> InRequestIQ -> m (Maybe (Either StanzaError [Element]))
discoIqHandler (DiscoPlugin {..}) (InRequestIQ { iriType = IQGet, iriChildren = [req] }) =
  case res of
    Nothing -> return Nothing
    Just Nothing -> return $ Just $ Left $ itemNotFound "discoIqHandler: unknown node"
    Just (Just (name, elems)) ->
      return $ Just $ Right [element name (maybeToList $ fmap ("node", ) $ getAttr "node" req) $ map NodeElement elems]

  where infoName = discoInfoName "query"
        itemsName = discoItemsName "query"
        res
          | elementName req == infoName = Just $ fmap (infoName, ) $ case getAttr "node" req of
              Nothing -> Just $ emitDiscoEntity discoPEntity
              Just node | Just (entity, _) <- M.lookup node discoPChildren -> Just $ emitDiscoEntity entity
              _ -> Nothing
          | elementName req == itemsName = Just $ fmap (itemsName, ) $ case getAttr "node" req of
              Nothing -> Just $ emitDiscoItems discoPItems
              Just node | Just (_, items) <- M.lookup node discoPChildren -> Just $ emitDiscoItems items
              _ -> Nothing
          | otherwise = Nothing
discoIqHandler _ _ = return Nothing

discoPlugin :: MonadStream m => [DiscoPlugin] -> m (XMPPPlugin m)
discoPlugin plugins = do
  plugin <- case foldr (\a acc -> acc >>= discoPluginUnion a) (Just emptyDiscoPlugin) plugins of
    Nothing -> fail "discoPlugin: overlapping plugins"
    Just r -> return r
  return $ emptyPlugin { pluginRequestIqHandler = discoIqHandler plugin }
