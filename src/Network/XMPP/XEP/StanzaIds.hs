{-# LANGUAGE Strict #-}

-- | XEP-0359: Unique and Stable Stanza IDs
module Network.XMPP.XEP.StanzaIds (
  StanzaIdBy,
  StanzaIds (..),
  stanzaIdsPlugin,
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Registry as Reg
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Text.XML

import Network.XMPP.Address
import Network.XMPP.Filter (FilterReceive (..), FilterSend (..))
import qualified Network.XMPP.Filter as Filter
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XML

sidName :: Text -> Name
(_, sidName) = namePair "urn:xmpp:sid:0"

-- | The @by@ attribute of a stanza-id (typically a server or MUC JID).
type StanzaIdBy = Text

-- | Parsed XEP-0359 IDs from a stanza.
data StanzaIds = StanzaIds
  { sidsOriginId :: Maybe StanzaId
  , sidsStanzaIds :: Map StanzaIdBy StanzaId
  }
  deriving (Show, Eq)

tryParseOriginId :: Element -> Either StanzaError (Maybe StanzaId)
tryParseOriginId e
  | elementName e == sidName "origin-id" = case getAttr "id" e of
      Nothing -> Left $ badRequest "missing id attribute in <origin-id/>"
      Just oid -> Right (Just oid)
  | otherwise = Right Nothing

tryParseStanzaId :: Element -> Either StanzaError (Maybe (StanzaIdBy, StanzaId))
tryParseStanzaId e
  | elementName e == sidName "stanza-id" = case (getAttr "id" e, getAttr "by" e) of
      (Just sid, Just by) -> Right (Just (by, sid))
      _ -> Left $ badRequest "missing id/by attribute in <stanza-id/>"
  | otherwise = Right Nothing

originIdElement :: StanzaId -> Element
originIdElement oid = element (sidName "origin-id") [("id", oid)] []

extractStanzaIds :: [Element] -> Either StanzaError (StanzaIds, [Element])
extractStanzaIds elems = do
  let (oidElems, rest1) = partition ((== sidName "origin-id") . elementName) elems
      (sidElems, rest2) = partition ((== sidName "stanza-id") . elementName) rest1
  oids <- traverse tryParseOriginId oidElems
  sids <- traverse tryParseStanzaId sidElems
  let sidsOriginId = listToMaybe (catMaybes oids)
      sidsStanzaIds = M.fromList (catMaybes sids)
  return (StanzaIds {..}, rest2)

data StanzaIdsPlugin = StanzaIdsPlugin

{- | Message filter: on receive, extract origin-id and stanza-ids into
extended. On send, generate a fresh origin-id and insert it.
-}
instance (MonadStream m) => FilterReceive m XMPPAddress IMMessage StanzaError StanzaIdsPlugin where
  filterReceive _ _ msg = case extractStanzaIds (imRaw msg) of
    Left err -> do
      $(logError) [i|XEP-0359 stanza IDs: #{err}|]
      return $ Right msg
    Right (ids, raw') ->
      let ext' = Reg.insert ids (imExtended msg)
       in return $ Right $ msg {imRaw = raw', imExtended = ext'}

instance (MonadStream m) => FilterSend m XMPPAddress IMMessage StanzaError StanzaIdsPlugin where
  filterSend _ _ msg = do
    oid <- liftIO $ UUID.toText <$> UUID.nextRandom
    let ext = imExtended msg
        (_, ext') = Reg.pop (Proxy :: Proxy StanzaIds) ext
        raw = imRaw msg
        raw' = originIdElement oid : raw
        mid = imId msg <|> Just oid
     in return $ Right $ msg {imId = mid, imRaw = raw', imExtended = ext'}

stanzaIdsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
stanzaIdsPlugin pluginsRef = do
  imp <- getIMPlugin pluginsRef
  Filter.pushNewOrFailM StanzaIdsPlugin (imPluginReceiveFilters imp)
  Filter.pushNewOrFailM StanzaIdsPlugin (imPluginSendFilters imp)
