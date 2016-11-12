module Network.XMPP.XEP.Disco
  ( DiscoIdentity(..)
  , DiscoFeature
  , DiscoEntity(..)
  , getDiscoEntity
  , DiscoItems
  , getDiscoItems
  , DiscoTopo(..)
  , getDiscoTopo
  , discoPlugin
  ) where

import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Text.InterpolatedString.Perl6 (qq)
import Data.Default.Class

import Network.XMPP.Utils
import Network.XMPP.XML
import Network.XMPP.Plugin
import Network.XMPP.Session
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
                 deriving (Show, Eq)

instance Default DiscoEntity where
  def = DiscoEntity { discoIdentities = M.empty
                    , discoFeatures = S.empty
                    }

discoInfoName :: Text -> Name
discoInfoName = nsName "http://jabber.org/protocol/disco#info"

requiredAttr :: Name -> Element -> Either StanzaError Text
requiredAttr name e = case getAttr name e of
  Nothing -> Left $ badRequest [qq|requiredAttr: no $name attribute in {elementName e}|]
  Just r -> return r

parseNamed :: (Show k, Ord k) => Element -> Name -> (Element -> Either StanzaError k) -> Either StanzaError (Map k (Maybe LocalizedText))
parseNamed root name getKey = do
  items <- mapM getElem $ fromElement root $/ XC.element name &| curElement
  namedMap <- sequence $ fmap sequence $ M.fromListWithKey (\k -> M.unionWith $ conflict k) $ fmap (second $ fmap return) items
  return $ fmap localizedFromText namedMap
  
  where getElem e = do
          k <- getKey e
          let names = case getAttr "name" e of
                Nothing -> M.empty
                Just text -> M.singleton (xmlLangGet e) text
          return (k, names)
        conflict feature _ _ = Left $ badRequest [qq|parseNamed: conflicting feature $feature|]

parseDiscoEntity :: Element -> Either StanzaError DiscoEntity
parseDiscoEntity re = do
  discoIdentities <- parseNamed re (discoInfoName "identity") getIdentity
  features <- mapM getFeature $ fromElement re $/ XC.element (discoInfoName "feature") &| curElement

  return DiscoEntity { discoFeatures = S.fromList features
                     , ..
                     }
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
    Right [r] -> parseDiscoEntity r
    _ -> Left $ badRequest "getDiscoEntity: multiple elements in response"

type DiscoNode = Text
type DiscoItems = Map (XMPPAddress, Maybe DiscoNode) (Maybe LocalizedText)

discoItemsName :: Text -> Name
discoItemsName = nsName "http://jabber.org/protocol/disco#items"

parseDiscoItems :: Element -> Either StanzaError DiscoItems
parseDiscoItems re = parseNamed re (discoItemsName "item") getItem
  where getItem e = do
          address' <- requiredAttr "jid" e
          address <- case parseValue xmppAddress address' of
            Nothing -> Left $ jidMalformed [qq|parseDiscoItems: malformed jid {address'}|]
            Just r -> return r
          let node = getAttr "node" e
          return (address, node)

getDiscoItems :: MonadStream m => StanzaSession m -> XMPPAddress -> m (Either StanzaError DiscoItems)
getDiscoItems sess addr = do
  ret <- stanzaSyncRequest sess OutRequestIQ { oriTo = Just addr
                                            , oriIqType = IQGet
                                            , oriChildren = [closedElement $ discoItemsName "query"]
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
getDiscoTopo sess addr node = runEitherT $ do
  discoRoot <- EitherT $ getDiscoEntity sess addr node
  case node of
    Nothing -> do
      items <- EitherT $ getDiscoItems sess addr
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

discoIqHandler :: MonadSession m =>  DiscoEntity -> DiscoItems -> InRequestIQ -> m (Maybe (Either StanzaError [Element]))
discoIqHandler (DiscoEntity {..}) items (InRequestIQ { iriType = IQSet, iriChildren = [req] })
  | elementName req == discoInfoName "query" = Just <$> do
      let makeIdentityAttr (DiscoIdentity {..}) = [("category", discoCategory), ("type", discoType)]
          identities = emitNamed discoIdentities (discoInfoName "identity") makeIdentityAttr

          features = map (\f -> element (discoInfoName "feature") [("var", f)] []) $ S.toAscList discoFeatures

      return $ Right $ identities ++ features
  | elementName req == discoItemsName "query" = Just <$> do
      let makeItemAttr (addr, mNode) = [("jid", addressToText addr)] ++ maybeToList (fmap ("node", ) mNode)
      return $ Right $ emitNamed items (discoItemsName "item") makeItemAttr
discoIqHandler _  _ _ = return Nothing

discoPlugin :: MonadSession m => DiscoEntity -> DiscoItems -> XMPPPlugin m
discoPlugin entity items = def { pluginRequestIqHandler = discoIqHandler entity items }
