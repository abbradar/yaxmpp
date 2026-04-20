{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Version (
  VersionInfo (..),
  VersionPlugin,
  defaultVersion,
  getVersionPlugin,
  getVersion,
  versionPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.MemoAsync (MemoAsync)
import qualified Control.MemoAsync as MemoAsync
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Registry as Reg
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

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Session (sessionAddress)
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

-- | Lazy version cache stored in each presence's extended registry.
newtype LazyVersion m = LazyVersion (MemoAsync m (Either StanzaError VersionInfo))

instance Show (LazyVersion m) where
  show _ = "LazyVersion"

data VersionPlugin m = VersionPlugin
  { versionPluginSession :: StanzaSession m
  , versionPluginInfo :: VersionInfo
  , versionPluginPresencePlugin :: PresencePlugin m
  , versionPluginHome :: MemoAsync m (Either StanzaError VersionInfo)
  , versionPluginMyAddress :: FullJID
  }

{- | The 'VersionPlugin' is itself a presence codec: on decode, it attaches a
'LazyVersion' for the full JID; on encode, it strips it.
-}
instance (MonadStream m) => Codec m FullJID Presence (VersionPlugin m) where
  codecDecode (VersionPlugin {versionPluginSession}) faddr pres = do
    let addr = fullJidAddress faddr
    lazy <- MemoAsync.new $ doGetVersion versionPluginSession addr
    let lv = LazyVersion lazy :: LazyVersion m
    return $ pres {presenceExtended = Reg.insert lv (presenceExtended pres)}
  codecEncode _ _ pres =
    return $ pres {presenceExtended = Reg.delete (Proxy :: Proxy (LazyVersion m)) (presenceExtended pres)}

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

-- | Always perform a fresh version request (no caching).
doGetVersion :: (MonadStream m) => StanzaSession m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
doGetVersion sess addr handler =
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

{- | Get version info, using cached MemoAsync values for JIDs with
active presences and for the homeserver.
-}
getVersion :: (MonadStream m) => VersionPlugin m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
getVersion (VersionPlugin {versionPluginHome, versionPluginPresencePlugin, versionPluginMyAddress, versionPluginSession}) addr handler
  | isHomeServer = MemoAsync.get versionPluginHome handler
  | Just full <- fullJidGet addr = do
      presences <- getAllPresences versionPluginPresencePlugin
      case M.lookup full presences of
        Just pres
          | Just (LazyVersion lazy) <- Reg.lookup (Proxy :: Proxy (LazyVersion m)) (presenceExtended pres) ->
              MemoAsync.get lazy handler
        _ -> doGetVersion versionPluginSession addr handler
  | otherwise = doGetVersion versionPluginSession addr handler
 where
  isHomeServer = addressLocal addr == Nothing && addressResource addr == Nothing && addressDomain addr == bareDomain (fullBare versionPluginMyAddress)

instance (Typeable m) => DiscoInfoProvider (VersionPlugin m) where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton versionNS

versionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> VersionInfo -> m ()
versionPlugin pluginsRef settings = do
  let versionPluginSession = pluginsSession pluginsRef
      versionPluginMyAddress = sessionAddress $ ssSession versionPluginSession
      versionPluginInfo = settings
      homeAddr = XMPPAddress Nothing (bareDomain $ fullBare versionPluginMyAddress) Nothing
  versionPluginHome <- MemoAsync.new $ doGetVersion versionPluginSession homeAddr
  versionPluginPresencePlugin <- getPresencePlugin pluginsRef
  let plugin :: VersionPlugin m = VersionPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM plugin iqHandlers
  addDiscoInfo pluginsRef plugin
  Codec.pushNewOrFailM plugin (presencePluginCodecs versionPluginPresencePlugin)
