{-# LANGUAGE Strict #-}

-- | XEP-0421: Anonymous unique occupant identifiers for MUCs
module Network.XMPP.XEP.OccupantId (
  OccupantId (..),
  occupantIdPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Data.List (partition)
import Data.Maybe
import Data.Proxy
import qualified Data.Registry as Reg
import Data.Text (Text)
import Text.XML

import Network.XMPP.Address
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stream
import Network.XMPP.XML

occupantIdName :: Text -> Name
(_, occupantIdName) = namePair "urn:xmpp:occupant-id:0"

-- | A stable, pseudonymous occupant identifier assigned by the MUC service.
newtype OccupantId = OccupantId {occupantId :: Text}
  deriving (Show, Eq)

parseOccupantId :: Element -> Maybe OccupantId
parseOccupantId e
  | elementName e == occupantIdName "occupant-id" = OccupantId <$> getAttr "id" e
  | otherwise = Nothing

occupantIdElement :: OccupantId -> Element
occupantIdElement (OccupantId oid) = element (occupantIdName "occupant-id") [("id", oid)] []

extractOccupantId :: [Element] -> (Maybe OccupantId, [Element])
extractOccupantId elems =
  let (oidElems, rest) = partition ((== occupantIdName "occupant-id") . elementName) elems
   in (listToMaybe $ mapMaybe parseOccupantId oidElems, rest)

data OccupantIdCodec = OccupantIdCodec

instance (MonadStream m) => Codec m XMPPAddress IMMessage OccupantIdCodec where
  codecDecode _ _ msg =
    let (moid, raw') = extractOccupantId (imRaw msg)
        ext = imExtended msg
        ext' = maybe ext (\o -> Reg.insert o ext) moid
     in return $ msg {imRaw = raw', imExtended = ext'}

  codecEncode _ _ msg =
    let ext = imExtended msg
        (moid, ext') = Reg.pop (Proxy :: Proxy OccupantId) ext
        raw = imRaw msg
        raw' = maybe raw (\o -> occupantIdElement o : raw) moid
     in return $ msg {imRaw = raw', imExtended = ext'}

instance (MonadStream m) => Codec m FullJID Presence OccupantIdCodec where
  codecDecode _ _ pres =
    let (moid, raw') = extractOccupantId (presenceRaw pres)
        ext = presenceExtended pres
        ext' = maybe ext (\o -> Reg.insert o ext) moid
     in return $ pres {presenceRaw = raw', presenceExtended = ext'}

  codecEncode _ _ pres =
    let ext = presenceExtended pres
        (moid, ext') = Reg.pop (Proxy :: Proxy OccupantId) ext
        raw = presenceRaw pres
        raw' = maybe raw (\o -> occupantIdElement o : raw) moid
     in return $ pres {presenceRaw = raw', presenceExtended = ext'}

occupantIdPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
occupantIdPlugin pluginsRef = do
  mCodecs <- imCodecs pluginsRef
  Codec.pushNewOrFailM OccupantIdCodec mCodecs
  pCodecs <- presenceCodecs pluginsRef
  Codec.pushNewOrFailM OccupantIdCodec pCodecs
