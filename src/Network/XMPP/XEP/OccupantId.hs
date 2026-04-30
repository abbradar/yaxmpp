{-# LANGUAGE Strict #-}

-- | XEP-0421: Anonymous unique occupant identifiers for MUCs
module Network.XMPP.XEP.OccupantId (
  OccupantId (..),
  occupantIdPlugin,
) where

import Control.Monad.Logger
import Data.List (partition)
import qualified Data.Registry as Reg
import Data.String.Interpolate (i)
import Data.Text (Text)
import Text.XML

import Network.XMPP.Address
import Network.XMPP.Filter (FilterReceive (..))
import qualified Network.XMPP.Filter as Filter
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stanza (StanzaError)
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

extractOccupantId :: [Element] -> Either String (Maybe OccupantId, [Element])
extractOccupantId elems =
  let (oidElems, rest) = partition ((== occupantIdName "occupant-id") . elementName) elems
   in case oidElems of
        [] -> Right (Nothing, rest)
        (e : _) -> (\oid -> (Just oid, rest)) <$> parseOccupantId e

data OccupantIdPlugin = OccupantIdPlugin

{- | Receive-only filter: occupant IDs are stamped by the MUC service, so
the send side is a no-op.
-}
instance (MonadStream m) => FilterReceive m XMPPAddress IMMessage StanzaError OccupantIdPlugin where
  filterReceive _ _ msg = case extractOccupantId (imRaw msg) of
    Left err -> do
      $(logError) [i|XEP-0421 occupant ID: #{err}|]
      return $ Right msg
    Right (moid, raw') ->
      let ext = imExtended msg
          ext' = maybe ext (\o -> Reg.insert o ext) moid
       in return $ Right $ msg {imRaw = raw', imExtended = ext'}

instance (MonadStream m) => FilterReceive m FullJID Presence StanzaError OccupantIdPlugin where
  filterReceive _ _ pres = case extractOccupantId (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0421 occupant ID: #{err}|]
      return $ Right pres
    Right (moid, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\o -> Reg.insert o ext) moid
       in return $ Right $ pres {presenceRaw = raw', presenceExtended = ext'}

occupantIdPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
occupantIdPlugin pluginsRef = do
  imp <- getIMPlugin pluginsRef
  Filter.pushNewOrFailM OccupantIdPlugin (imPluginReceiveFilters imp)
  pp <- getPresencePlugin pluginsRef
  Filter.pushNewOrFailM OccupantIdPlugin (presencePluginReceiveFilters pp)
