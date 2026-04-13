{-# LANGUAGE Strict #-}

-- | XEP-0359: Unique and Stable Stanza IDs
module Network.XMPP.XEP.StanzaIds (
  OriginId (..),
  StanzaIdBy,
  StanzaIdValue,
  StanzaIds (..),
  stanzaIdsPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.Monad.IO.Class
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Registry as Reg
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Text.XML

import Network.XMPP.Address
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Stream
import Network.XMPP.XML

sidName :: Text -> Name
(_, sidName) = namePair "urn:xmpp:sid:0"

-- | Origin ID set by the originating entity.
newtype OriginId = OriginId {originId :: Text}
  deriving (Show, Eq)

-- | The @by@ attribute of a stanza-id (typically a server or MUC JID).
type StanzaIdBy = Text

-- | The @id@ attribute of a stanza-id.
type StanzaIdValue = Text

-- | Parsed XEP-0359 IDs from a stanza.
data StanzaIds = StanzaIds
  { sidsOriginId :: Maybe OriginId
  , sidsStanzaIds :: Map StanzaIdBy StanzaIdValue
  }
  deriving (Show, Eq)

parseOriginId :: Element -> Maybe OriginId
parseOriginId e
  | elementName e == sidName "origin-id" = OriginId <$> getAttr "id" e
  | otherwise = Nothing

parseStanzaId :: Element -> Maybe (StanzaIdBy, StanzaIdValue)
parseStanzaId e
  | elementName e == sidName "stanza-id" = do
      sid <- getAttr "id" e
      by <- getAttr "by" e
      return (by, sid)
  | otherwise = Nothing

originIdElement :: OriginId -> Element
originIdElement (OriginId oid) = element (sidName "origin-id") [("id", oid)] []

extractStanzaIds :: [Element] -> (StanzaIds, [Element])
extractStanzaIds elems =
  let (oidElems, rest1) = partition ((== sidName "origin-id") . elementName) elems
      (sidElems, rest2) = partition ((== sidName "stanza-id") . elementName) rest1
      sidsOriginId = listToMaybe $ mapMaybe parseOriginId oidElems
      sidsStanzaIds = M.fromList $ mapMaybe parseStanzaId sidElems
   in (StanzaIds {..}, rest2)

data StanzaIdsCodec = StanzaIdsCodec

{- | Message codec: on decode, extract origin-id and stanza-ids into extended.
On encode, generate a fresh origin-id and insert it.
-}
instance (MonadStream m) => Codec m XMPPAddress IMMessage StanzaIdsCodec where
  codecDecode _ _ msg =
    let (ids, raw') = extractStanzaIds (imRaw msg)
        ext = imExtended msg
        ext' = Reg.insert ids ext
     in return $ msg {imRaw = raw', imExtended = ext'}

  codecEncode _ _ msg = do
    oid <- liftIO $ OriginId . UUID.toText <$> UUID.nextRandom
    let ext = imExtended msg
        (_, ext') = Reg.pop (Proxy :: Proxy StanzaIds) ext
        raw = imRaw msg
        raw' = originIdElement oid : raw
     in return $ msg {imRaw = raw', imExtended = ext'}

stanzaIdsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
stanzaIdsPlugin pluginsRef = do
  codecs <- imCodecs pluginsRef
  Codec.pushNewOrFailM StanzaIdsCodec codecs
