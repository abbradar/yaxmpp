{-# LANGUAGE Strict #-}

-- | XEP-0421: Anonymous unique occupant identifiers for MUCs
module Network.XMPP.XEP.OccupantId (
  OccupantId (..),
  occupantIdPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.Monad.Logger
import Data.List (partition)
import Data.Proxy
import qualified Data.Registry as Reg
import Data.String.Interpolate (i)
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

parseOccupantId :: Element -> Either String OccupantId
parseOccupantId e = case getAttr "id" e of
  Nothing -> Left "missing id attribute on <occupant-id>"
  Just oid -> Right $ OccupantId oid

occupantIdElement :: OccupantId -> Element
occupantIdElement (OccupantId oid) = element (occupantIdName "occupant-id") [("id", oid)] []

extractOccupantId :: [Element] -> Either String (Maybe OccupantId, [Element])
extractOccupantId elems =
  let (oidElems, rest) = partition ((== occupantIdName "occupant-id") . elementName) elems
   in case oidElems of
        [] -> Right (Nothing, rest)
        (e : _) -> (\oid -> (Just oid, rest)) <$> parseOccupantId e

data OccupantIdPlugin = OccupantIdPlugin

instance (MonadStream m) => Codec m XMPPAddress IMMessage OccupantIdPlugin where
  codecDecode _ _ msg = case extractOccupantId (imRaw msg) of
    Left err -> do
      $(logError) [i|XEP-0421 occupant ID: #{err}|]
      return msg
    Right (moid, raw') ->
      let ext = imExtended msg
          ext' = maybe ext (\o -> Reg.insert o ext) moid
       in return $ msg {imRaw = raw', imExtended = ext'}

  codecEncode _ _ msg =
    let ext = imExtended msg
        (moid, ext') = Reg.pop (Proxy :: Proxy OccupantId) ext
        raw = imRaw msg
        raw' = maybe raw (\o -> occupantIdElement o : raw) moid
     in return $ msg {imRaw = raw', imExtended = ext'}

instance (MonadStream m) => Codec m FullJID Presence OccupantIdPlugin where
  codecDecode _ _ pres = case extractOccupantId (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0421 occupant ID: #{err}|]
      return pres
    Right (moid, raw') ->
      let ext = presenceExtended pres
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
  imp <- getIMPlugin pluginsRef
  Codec.pushNewOrFailM OccupantIdPlugin (imPluginCodecs imp)
  pp <- getPresencePlugin pluginsRef
  Codec.pushNewOrFailM OccupantIdPlugin (presencePluginCodecs pp)
