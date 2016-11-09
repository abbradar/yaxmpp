module Network.XMPP.XEP.Disco
  ( DiscoIdentity(..)
  , DiscoName
  , DiscoFeature
  , DiscoEntity(..)
  , getDiscoEntity
  , DiscoItems
  , getDiscoItems
  , DiscoTopo(..)
  , getDiscoTopo
  ) where

import Control.Applicative
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

import Network.XMPP.XML
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Address

data DiscoIdentity = DiscoIdentity { discoCategory :: Text
                                   , discoType :: Text
                                   }
                   deriving (Show, Eq, Ord)

type DiscoName = Text
type DiscoFeature = Text

data DiscoEntity = DiscoEntity { discoIdentities :: Map DiscoIdentity (Map XMLLang DiscoName) -- ^ Map of identities to their human-readable descriptions.
                               , discoFeatures :: Set DiscoFeature
                               }
                 deriving (Show, Eq)

discoInfoName :: Text -> Name
discoInfoName = nsName "http://jabber.org/protocol/disco#info"

requiredAttr :: Name -> Element -> Either StanzaError Text
requiredAttr name e = case getAttr name e of
  Nothing -> Left $ badRequest [qq|requiredAttr: no $name attribute in {elementName e}|]
  Just r -> return r

parseDiscoEntity :: Element -> Either StanzaError DiscoEntity
parseDiscoEntity re = do
  identities <- mapM getIdentity $ fromElement re $/ XC.element (discoInfoName "identity") &| curElement
  features <- mapM getFeature $ fromElement re $/ XC.element (discoInfoName "feature") &| curElement

  return DiscoEntity { discoIdentities = M.fromListWith M.union identities
                     , discoFeatures = S.fromList features
                     }
  where getIdentity e = do
          discoCategory <- requiredAttr "category" e
          discoType <- requiredAttr "type" e
          let names = case getAttr "name" e of
                Nothing -> M.empty
                Just name -> M.singleton (getAttr (xmlName "lang") e) name

          return (DiscoIdentity {..}, names)

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
type DiscoItems = Map (XMPPAddress, Maybe DiscoNode) (Maybe DiscoName)

discoItemsName :: Text -> Name
discoItemsName = nsName "http://jabber.org/protocol/disco#items"

parseDiscoItems :: Element -> Either StanzaError DiscoItems
parseDiscoItems re = fmap (M.fromListWith (<|>)) $ mapM getItem $ fromElement re $/ XC.element (discoItemsName "item") &| curElement
  where getItem e = do
          address' <- requiredAttr "jid" e
          address <- case readXMPPAddress address' of
            Nothing -> Left $ jidMalformed [qq|parseDiscoItems: malformed jid {address'}|]
            Just r -> return r
          let name = getAttr "name" e
          let node = getAttr "node" e
          return ((address, node), name)

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
                           , discoItems :: Map (XMPPAddress, Maybe DiscoNode) (Maybe DiscoName, Either StanzaError DiscoTopo)
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
