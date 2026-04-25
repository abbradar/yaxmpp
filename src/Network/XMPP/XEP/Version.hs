{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Version (
  VersionInfo (..),
  VersionPlugin,
  versionPluginSession,
  versionPluginCacheHandlers,
  VersionCacheHandlers,
  defaultVersion,
  getVersionPlugin,
  getVersion,
  requestVersion,
  versionPlugin,
) where

import Control.HandlerList (Handler (..), HandlerList)
import qualified Control.HandlerList as HL
import Data.Maybe
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Version
import System.Info (os)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

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

{- | Handler list for resolving version requests from cache. If a handler
matches the request, it must fire the callback itself and return @Just ()@;
otherwise return @Nothing@ to fall through.
-}
type VersionCacheHandlers m = HandlerList m (XMPPAddress, Either StanzaError VersionInfo -> m ()) ()

data VersionPlugin m = VersionPlugin
  { versionPluginSession :: StanzaSession m
  , versionPluginInfo :: VersionInfo
  , versionPluginCacheHandlers :: VersionCacheHandlers m
  }

instance (MonadStream m) => Handler m InRequestIQ RequestIQResponse (VersionPlugin m) where
  tryHandle (VersionPlugin {versionPluginInfo = VersionInfo {..}}) (InRequestIQ {iriType = IQGet, iriChildren = [req]})
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

-- | Direct version IQ fetch — bypasses the cache handler chain.
requestVersion :: (MonadStream m) => StanzaSession m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
requestVersion sess addr handler =
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
      _ -> Left $ badRequest "invalid version response"
 where
  getEntry r name = fromElement r $/ XC.element (versionName name) &/ content

getVersionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (VersionPlugin m)
getVersionPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (VersionPlugin m)) $ pluginsHooksSet pluginsRef

{- | Get version info, first consulting the cache handlers; falls back to a
direct IQ if no handler claims the request.
-}
getVersion :: (MonadStream m) => VersionPlugin m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
getVersion (VersionPlugin {..}) addr handler = do
  handled <- HL.call versionPluginCacheHandlers (addr, handler)
  case handled of
    Just () -> return ()
    Nothing -> requestVersion versionPluginSession addr handler

instance (Typeable m) => DiscoInfoProvider (VersionPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton versionNS

versionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> VersionInfo -> m ()
versionPlugin pluginsRef settings = do
  versionPluginCacheHandlers <- HL.new :: m (VersionCacheHandlers m)
  let versionPluginSession = pluginsSession pluginsRef
      versionPluginInfo = settings
      plugin :: VersionPlugin m = VersionPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsIQHandlers pluginsRef
  addDiscoInfo pluginsRef plugin
