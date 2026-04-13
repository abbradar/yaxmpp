{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Version (
  VersionInfo (..),
  defaultVersion,
  getVersion,
  versionPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.MemoAsync (MemoAsync)
import qualified Control.MemoAsync as MemoAsync
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
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

-- | Homeserver version cache.
newtype VersionHomeCache m = VersionHomeCache (MemoAsync m (Either StanzaError VersionInfo))

-- | Codec that creates a LazyVersion on decode and strips it on encode.
data VersionCodec m = VersionCodec (XMPPPluginsRef m)

instance (MonadStream m) => Codec m FullJID Presence (VersionCodec m) where
  codecDecode (VersionCodec pluginsRef) faddr pres = do
    let addr = fullJidAddress faddr
    lazy <- MemoAsync.new $ doGetVersion pluginsRef addr
    let lv = LazyVersion lazy :: LazyVersion m
    return $ pres {presenceExtended = Reg.insert lv (presenceExtended pres)}
  codecEncode _ _ pres =
    return $ pres {presenceExtended = Reg.delete (Proxy :: Proxy (LazyVersion m)) (presenceExtended pres)}

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

-- | Always perform a fresh version request (no caching).
doGetVersion :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
doGetVersion pluginsRef addr handler = do
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

{- | Get version info, using cached MemoAsync values for JIDs with
active presences and for the homeserver.
-}
getVersion :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> (Either StanzaError VersionInfo -> m ()) -> m ()
getVersion pluginsRef addr handler
  | isHomeServer = do
      VersionHomeCache lazy <- RegRef.lookupOrFailM (Proxy :: Proxy (VersionHomeCache m)) $ pluginsHooksSet pluginsRef
      MemoAsync.get lazy handler
  | Just full <- fullJidGet addr = do
      presences <- getAllPresences pluginsRef
      case M.lookup full presences of
        Just pres
          | Just (LazyVersion lazy) <- Reg.lookup (Proxy :: Proxy (LazyVersion m)) (presenceExtended pres) ->
              MemoAsync.get lazy handler
        _ -> doGetVersion pluginsRef addr handler
  | otherwise = doGetVersion pluginsRef addr handler
 where
  myAddress = sessionAddress $ ssSession $ pluginsSession pluginsRef
  isHomeServer = addressLocal addr == Nothing && addressResource addr == Nothing && addressDomain addr == bareDomain (fullBare myAddress)

data VersionDisco = VersionDisco

instance DiscoInfoProvider VersionDisco where
  discoProviderInfo _ = featuresDiscoInfo Nothing $ S.singleton versionNS

versionPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> VersionInfo -> m ()
versionPlugin pluginsRef settings = do
  let myAddress = sessionAddress $ ssSession $ pluginsSession pluginsRef
      homeAddr = XMPPAddress Nothing (bareDomain $ fullBare myAddress) Nothing
  homeLazy <- MemoAsync.new $ doGetVersion pluginsRef homeAddr
  iqHandlers <- pluginsIQHandlers pluginsRef
  HL.pushNewOrFailM (VersionPlugin settings) iqHandlers
  addDiscoInfo pluginsRef VersionDisco
  RegRef.insertNewOrFailM (VersionHomeCache homeLazy :: VersionHomeCache m) $ pluginsHooksSet pluginsRef
  codecs <- presenceCodecs pluginsRef
  Codec.pushNewOrFailM (VersionCodec pluginsRef :: VersionCodec m) codecs
