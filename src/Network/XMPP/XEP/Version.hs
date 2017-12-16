module Network.XMPP.XEP.Version
  ( VersionInfo
  , getVersion
  , versionPlugin
  ) where

import Data.Maybe
import System.Info (os)
import Data.Version
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Default.Class
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

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

instance Default VersionInfo where
  def = VersionInfo { swName = "yaxmpp"
                    , swVersion = T.pack $ showVersion version
                    , swOS = Just $ T.pack os
                    }

versionIqHandler :: MonadStream m => VersionInfo -> InRequestIQ -> m (Maybe (Either StanzaError [Element]))
versionIqHandler (VersionInfo {..}) (InRequestIQ { iriType = IQGet, iriChildren = [req] })
  | elementName req == queryTag =
      return $ Just $ Right [element queryTag [] $ map (\(name, value) -> NodeElement $ element (versionName name) [] [NodeContent value]) result]
  where queryTag = versionName "query"
        result = [ ("name", swName)
                 , ("version", swVersion)
                 ] ++ maybeToList (fmap ("os", ) swOS)
versionIqHandler _ _ = return Nothing

getVersion :: MonadStream m => StanzaSession m -> XMPPAddress -> m (Either StanzaError VersionInfo)
getVersion sess addr = do
  ret <- stanzaSyncRequest sess OutRequestIQ { oriTo = Just addr
                                            , oriIqType = IQGet
                                            , oriChildren = [closedElement (versionName "query")]
                                            }
  return $ case ret of
    Left (e, _) -> Left e
    Right [r] | elementName r == versionName "query"
              , [swName] <- getEntry r "name"
              , [swVersion] <- getEntry r "version"
              , swOS <- listToMaybe $ getEntry r "os"
                -> Right $ VersionInfo {..}
    _ -> Left $ badRequest "getVersion: invalid response"

  where getEntry r name = fromElement r $/ XC.element (versionName name) &/ content

versionPlugin :: MonadStream m => VersionInfo -> m (XMPPPlugin m, DiscoPlugin)
versionPlugin settings = do
  let xmppPlugin = def { pluginRequestIqHandler = versionIqHandler settings
                       }
      discoHandler = def { discoPEntity = def { discoFeatures = S.singleton versionNS }
                         }
  return (xmppPlugin, discoHandler)
