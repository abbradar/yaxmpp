{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Version (
  VersionInfo (..),
  defaultVersion,
  getVersion,
  versionPlugin,
) where

import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import System.Info (os)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import qualified Data.RefMap as RefMap
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XML
import Paths_yaxmpp (version)

versionNS :: Text
versionName :: Text -> Name
(versionNS, versionName) = namePair "jabber:iq:version"

data VersionInfo = VersionInfo
  { swName :: Text
  , swVersion :: Text
  , swOS :: Maybe Text
  }
  deriving (Show, Eq)

defaultVersion :: VersionInfo
defaultVersion =
  VersionInfo
    { swName = "yaxmpp"
    , swVersion = T.pack $ showVersion version
    , swOS = Just $ T.pack os
    }

newtype VersionPlugin = VersionPlugin VersionInfo

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse VersionPlugin where
  tryHandle (VersionPlugin (VersionInfo {..})) (InRequestIQ {iriType = IQGet, iriChildren = [req]})
    | elementName req == queryTag =
        return $ Just $ IQResult [element queryTag [] $ map (\(name, value) -> NodeElement $ element (versionName name) [] [NodeContent value]) result]
   where
    queryTag = versionName "query"
    result =
      [ ("name", swName)
      , ("version", swVersion)
      ]
        ++ maybeToList (fmap ("os",) swOS)
  tryHandle _ _ = return Nothing

getVersion :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
getVersion pluginsRef addr handler = do
  let sess = pluginsSession pluginsRef
  stanzaRequest
    sess
    OutRequestIQ
      { oriTo = Just addr
      , oriIqType = IQGet
      , oriChildren = [closedElement (versionName "query")]
      }
    $ \resp -> handler $ case resp of
        Left e -> Left e
        Right [r]
          | elementName r == versionName "query"
          , [swName] <- getEntry r "name"
          , [swVersion] <- getEntry r "version"
          , swOS <- listToMaybe $ getEntry r "os" ->
              Right $ VersionInfo {..}
        _ -> Left $ badRequest "getVersion: invalid response"
 where
  getEntry r name = fromElement r $/ XC.element (versionName name) &/ content

versionPlugin :: (MonadStream m) => XMPPPluginsRef m -> VersionInfo -> m ()
versionPlugin pluginsRef settings = do
  let discoInfo =
        emptyDiscoInfo
          { discoIEntity = emptyDiscoEntity {discoFeatures = S.singleton versionNS}
          }
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM (VersionPlugin settings) iqHandlers
  dInfos <- discoInfos pluginsRef
  void $ RefMap.add dInfos $ return discoInfo
