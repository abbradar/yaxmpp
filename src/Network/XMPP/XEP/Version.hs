{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Version
  ( VersionInfo(..)
  , defaultVersion
  , getVersion
  , versionPlugin
  ) where

import Data.Maybe
import System.Info (os)
import Data.Version
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import qualified Control.HandlerList as HandlerList
import qualified Data.RefMap as RefMap
import Paths_yaxmpp (version)
import Network.XMPP.XML
import Network.XMPP.Stream
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.XEP.Disco

versionNS :: Text
versionName :: Text -> Name
(versionNS, versionName) = namePair "jabber:iq:version"

data VersionInfo = VersionInfo { swName :: Text
                               , swVersion :: Text
                               , swOS :: Maybe Text
                               }
                 deriving (Show, Eq)

defaultVersion :: VersionInfo
defaultVersion = VersionInfo { swName = "yaxmpp"
                             , swVersion = T.pack $ showVersion version
                             , swOS = Just $ T.pack os
                             }

versionIQHandler :: MonadStream m => VersionInfo -> InRequestIQ -> m (Maybe RequestIQResponse)
versionIQHandler (VersionInfo {..}) (InRequestIQ { iriType = IQGet, iriChildren = [req] })
  | elementName req == queryTag =
      return $ Just $ IQResult [element queryTag [] $ map (\(name, value) -> NodeElement $ element (versionName name) [] [NodeContent value]) result]
  where queryTag = versionName "query"
        result = [ ("name", swName)
                 , ("version", swVersion)
                 ] ++ maybeToList (fmap ("os", ) swOS)
versionIQHandler _ _ = return Nothing

getVersion :: MonadStream m => StanzaSession m -> XMPPAddress -> m (Either StanzaError VersionInfo)
getVersion sess addr = do
  ret <- stanzaSyncRequest sess OutRequestIQ { oriTo = Just addr
                                            , oriIqType = IQGet
                                            , oriChildren = [closedElement (versionName "query")]
                                            }
  return $ case ret of
    Left e -> Left e
    Right [r] | elementName r == versionName "query"
              , [swName] <- getEntry r "name"
              , [swVersion] <- getEntry r "version"
              , swOS <- listToMaybe $ getEntry r "os"
                -> Right $ VersionInfo {..}
    _ -> Left $ badRequest "getVersion: invalid response"

  where getEntry r name = fromElement r $/ XC.element (versionName name) &/ content

versionPlugin :: MonadStream m => XMPPPluginsRef m -> DiscoRef m -> VersionInfo -> m ()
versionPlugin pluginsRef discoRef settings = do
  let discoInfo = emptyDiscoInfo { discoIEntity = emptyDiscoEntity { discoFeatures = S.singleton versionNS }
                                 }
  void $ HandlerList.add (pluginIQHandlers pluginsRef) $ versionIQHandler settings
  void $ RefMap.add (discoInfos discoRef) $ return discoInfo
