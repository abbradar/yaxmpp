{-# LANGUAGE Strict #-}

-- | XEP-0115: Entity Capabilities and XEP-0390: Entity Capabilities 2.0
module Network.XMPP.XEP.Capabilities (
  -- * XEP-0115
  CapsInstance (..),
  CapsInfo (..),

  -- * XEP-0390
  Caps2Info (..),

  -- * Plugin
  CapsPlugin,
  getCapsPlugin,
  capsPlugin,
) where

import Control.Applicative ((<|>))
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.List (partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import UnliftIO.IORef

import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
import Network.XMPP.Address (FullJID, XMPPAddress, fullJidGet, toXMPPAddress)
import Network.XMPP.Filter (Filter (..))
import qualified Network.XMPP.Filter as Filter
import Network.XMPP.Language (LocalizedText, localTexts)
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Session (sessionStreamFeatures)
import Network.XMPP.Stanza (StanzaError, ssBare, ssServer, ssSession)
import Network.XMPP.Stream
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.Hash
import Network.XMPP.XML

-- * XEP-0115: Entity Capabilities

_capsNS :: Text
capsName :: Text -> Name
(_capsNS, capsName) = namePair "http://jabber.org/protocol/caps"

data CapsInstance = CapsInstance
  { capsHash :: HashAlgo
  , capsNode :: Text
  , capsVer :: HashValue
  }
  deriving (Show, Eq, Ord)

{- | XEP-0115 capabilities parsed from presence. May contain multiple entries
from multiple @\<c\>@ elements with different hash algorithms or nodes.
-}
newtype CapsInfo = CapsInfo
  { capsInstances :: [CapsInstance]
  }
  deriving (Show, Eq)

parseCapsInstance :: Element -> Either String CapsInstance
parseCapsInstance e = do
  capsHash <- maybe (Left "missing hash attribute on <c>") Right $ getAttr "hash" e
  capsNode <- maybe (Left "missing node attribute on <c>") Right $ getAttr "node" e
  capsVer <- maybe (Left "missing ver attribute on <c>") Right $ getAttr "ver" e
  return CapsInstance {..}

capsInstanceToElement :: CapsInstance -> Element
capsInstanceToElement (CapsInstance {..}) =
  Element
    { elementName = capsName "c"
    , elementAttributes =
        M.fromList
          [ ("hash", capsHash)
          , ("node", capsNode)
          , ("ver", capsVer)
          ]
    , elementNodes = []
    }

-- * XEP-0390: Entity Capabilities 2.0

_caps2NS :: Text
caps2Name :: Text -> Name
(_caps2NS, caps2Name) = namePair "urn:xmpp:caps"

-- | XEP-0390 capabilities: a map from hash algorithm to verification hash.
newtype Caps2Info = Caps2Info
  { caps2Hashes :: Map HashAlgo HashValue
  }
  deriving (Show, Eq)

parseCaps2Info :: Element -> Either String Caps2Info
parseCaps2Info e =
  let cur = fromElement e
      hashes = M.fromList $ mapMaybe parseHashElement $ cur $/ XC.element (hashesName "hash") &| curElement
   in if M.null hashes then Left "no hashes in <c xmlns=\"urn:xmpp:caps\">" else Right Caps2Info {caps2Hashes = hashes}

caps2InfoToElement :: Caps2Info -> Element
caps2InfoToElement (Caps2Info {..}) =
  element (caps2Name "c") [] $
    map (NodeElement . uncurry hashElement) (M.toList caps2Hashes)

-- * Verification strings

-- | UTF-8 encode 'Text' into a 'BSB.Builder'.
encUtf8 :: Text -> Builder
encUtf8 = BSB.byteString . T.encodeUtf8

{- | A single (category, type, lang, name) tuple as used by the XEP-0115 and
XEP-0390 verification-string algorithms. Empty 'Text' represents an absent
value.
-}
data ExpandedIdentity = ExpandedIdentity
  { eiCategory :: Text
  , eiType :: Text
  , eiLang :: Text
  , eiName :: Text
  }
  deriving (Show, Eq, Ord)

{- | Expand 'discoIdentities' into one 'ExpandedIdentity' per (lang, name)
pair (or a single empty-name entry when no name is present).
-}
expandIdentities :: Map DiscoIdentity (Maybe LocalizedText) -> [ExpandedIdentity]
expandIdentities = concatMap expandOne . M.toList
 where
  expandOne (DiscoIdentity {discoCategory, discoType}, mNames) = case mNames of
    Nothing -> [ExpandedIdentity discoCategory discoType "" ""]
    Just lts -> map fromLang $ M.toList $ localTexts lts
     where
      fromLang (mLang, name) = ExpandedIdentity discoCategory discoType (fromMaybe "" mLang) name

{- | XEP-0115 §5.2 verification string for a 'DiscoEntity'. Forms (XEP-0128)
are not yet considered. Compute once per entity and feed to 'hashLazy' for
each algorithm.
-}
caps1Bytes :: DiscoEntity -> BSL.ByteString
caps1Bytes DiscoEntity {..} = BSB.toLazyByteString input
 where
  input = mconcat idChunks <> mconcat featChunks
  idChunks = map idChunk (sort $ expandIdentities discoIdentities)
  idChunk ExpandedIdentity {..} =
    encUtf8 eiCategory
      <> slash
      <> encUtf8 eiType
      <> slash
      <> encUtf8 eiLang
      <> slash
      <> encUtf8 eiName
      <> lt
  featChunks = map (\f -> encUtf8 f <> lt) (S.toAscList discoFeatures)
  slash = BSB.char7 '/'
  lt = BSB.char7 '<'

{- | XEP-0390 §5.4 verification string for a 'DiscoEntity'. Forms are not
yet considered (extensions component is empty). Compute once per entity
and feed to 'hashLazy' for each algorithm.
-}
caps2Bytes :: DiscoEntity -> BSL.ByteString
caps2Bytes DiscoEntity {..} = BSB.toLazyByteString input
 where
  input = identitiesPart <> sep <> featuresPart <> sep <> sep
  sep = BSB.word8 0x1c
  us = BSB.word8 0x1f
  rs = BSB.word8 0x1e

  identitiesPart = mconcat $ map identityChunk (sort $ expandIdentities discoIdentities)
  identityChunk ExpandedIdentity {..} =
    encUtf8 eiCategory
      <> us
      <> encUtf8 eiType
      <> us
      <> encUtf8 eiLang
      <> us
      <> encUtf8 eiName
      <> rs

  featuresPart = mconcat $ map featureChunk (S.toAscList discoFeatures)
  featureChunk f = encUtf8 f <> us <> rs

-- * Cache

{- | Which XEP a verification hash was advertised under. Used both as part
of the cache key (the verification string is XEP-specific) and to dispatch
the right verification string when validating a candidate.
-}
data CapsVer = Caps1 | Caps2
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CapsVer
instance FromJSON CapsVer

{- | Cache: hash-keyed disco entities. Persisted across runs. The key is
@(xep, algo, ver)@ — the verification string differs between XEP-0115
and XEP-0390, so a given @(algo, ver)@ pair could in principle map to
different disco entities under each XEP.
-}
data CapsCache = CapsCache
  { ccByHash :: Map (CapsVer, HashAlgo, HashValue) DiscoEntity
  }
  deriving (Show, Eq, Generic)

{- | A single cache row used purely for the JSON encoding (the in-memory
'CapsCache' uses a tuple-keyed 'Map' which Aeson can't encode directly).
-}
data SerializedCapsRow = SerializedCapsRow
  { scrXep :: CapsVer
  , scrAlgo :: HashAlgo
  , scrVer :: HashValue
  , scrEntry :: DiscoEntity
  }
  deriving (Show, Eq, Generic)

instance FromJSON SerializedCapsRow
instance ToJSON SerializedCapsRow

instance ToJSON CapsCache where
  toJSON CapsCache {..} =
    JSON.toJSON
      [ SerializedCapsRow {scrXep = xep, scrAlgo = a, scrVer = v, scrEntry = e}
      | ((xep, a, v), e) <- M.toList ccByHash
      ]

instance FromJSON CapsCache where
  parseJSON v = do
    rows <- JSON.parseJSON v :: JSON.Parser [SerializedCapsRow]
    return CapsCache {ccByHash = M.fromList [((scrXep, scrAlgo, scrVer), scrEntry) | SerializedCapsRow {..} <- rows]}

emptyCapsCache :: CapsCache
emptyCapsCache = CapsCache {ccByHash = M.empty}

-- * Plugin

data CapsPlugin m = CapsPlugin
  { capsPluginDisco :: DiscoPlugin m
  , capsPluginPresence :: PresencePlugin m
  , capsPluginCache :: IORef CapsCache
  , capsPluginOwnCaps :: IORef (Maybe (Maybe CapsInfo, Maybe Caps2Info))
  {- ^ Memoized caps for our own presence. 'Nothing' means we haven't
  computed yet; populated lazily on the first outgoing presence using
  whatever 'DiscoEntity' the disco plugin had merged at that moment.
  FIXME: caps are frozen at the moment of the first sent presence.
  Subsequent 'addDiscoInfo' calls that change the verification hash are
  silently ignored, so contacts will see a stale @ver@ until the next
  session. Register all 'DiscoInfoProvider's before sending the first
  presence to avoid this.
  -}
  , capsPluginNode :: Text
  -- ^ Node URI used in our XEP-0115 @\<c\>@.
  , capsPluginAlgos1 :: [SomeXMPPHashAlgorithm]
  {- ^ Hashers we publish in XEP-0115 @\<c\>@ elements. Resolved at plugin
  init from 'supportedHashAlgos', so the send path can't fail at runtime.
  -}
  , capsPluginAlgos2 :: [SomeXMPPHashAlgorithm]
  -- ^ Hashers we publish in our XEP-0390 @\<c\>@ element. Same as above.
  }

-- | Pure caps computation for a given 'DiscoEntity' under our advertised algorithms.
computeOwnCaps :: CapsPlugin m -> DiscoEntity -> (Maybe CapsInfo, Maybe Caps2Info)
computeOwnCaps CapsPlugin {capsPluginNode, capsPluginAlgos1, capsPluginAlgos2} entity =
  let bs1 = caps1Bytes entity
      bs2 = caps2Bytes entity
      caps1 =
        CapsInfo
          { capsInstances =
              [ CapsInstance {capsHash = someXMPPHashName some, capsNode = capsPluginNode, capsVer = hashLazyBase64 some bs1}
              | some <- capsPluginAlgos1
              ]
          }
      caps2 =
        Caps2Info
          { caps2Hashes = M.fromList [(someXMPPHashName some, hashLazyBase64 some bs2) | some <- capsPluginAlgos2]
          }
      mCaps1 = if null (capsInstances caps1) then Nothing else Just caps1
      mCaps2 = if M.null (caps2Hashes caps2) then Nothing else Just caps2
   in (mCaps1, mCaps2)

{- | Filter injects our currently-advertised XEP-0115/0390 caps into outgoing
presences and parses incoming caps into 'presenceExtended'. The first
outgoing presence triggers the verification-hash computation against
whatever 'DiscoEntity' the disco plugin had merged at that moment;
results are memoized for subsequent sends — later disco changes do
not propagate.
-}
instance (MonadStream m) => Filter m FullJID Presence StanzaError (CapsPlugin m) where
  filterReceive _ _ pres = case extractCaps (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0115/0390 entity capabilities: #{err}|]
      return $ Right pres
    Right (mcaps1, mcaps2, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\c -> Reg.insert c ext) mcaps1
          ext'' = maybe ext' (\c -> Reg.insert c ext') mcaps2
       in return $ Right $ pres {presenceRaw = raw', presenceExtended = ext''}

  filterSend cp@CapsPlugin {capsPluginDisco, capsPluginOwnCaps} _ pres = do
    cached <- readIORef capsPluginOwnCaps
    (mcaps1, mcaps2) <- case cached of
      Just pair -> return pair
      Nothing -> do
        merged <- readIORef (discoPluginMerged capsPluginDisco)
        let pair = computeOwnCaps cp (discoNEntity $ discoINode merged)
        atomicWriteIORef capsPluginOwnCaps (Just pair)
        return pair
    let raw = presenceRaw pres
        raw' = maybe raw (\c -> map capsInstanceToElement (capsInstances c) ++ raw) mcaps1
        raw'' = maybe raw' (\c -> caps2InfoToElement c : raw') mcaps2
    return $ Right $ pres {presenceRaw = raw''}

extractCaps :: [Element] -> Either String (Maybe CapsInfo, Maybe Caps2Info, [Element])
extractCaps elems = do
  let (caps1Elems, rest1) = partition (\e -> elementName e == capsName "c") elems
      (caps2Elems, rest2) = partition (\e -> elementName e == caps2Name "c") rest1
  caps1Items <- traverse parseCapsInstance caps1Elems
  let mcaps1 = case caps1Items of
        [] -> Nothing
        xs -> Just $ CapsInfo xs
  mcaps2 <- case caps2Elems of
    [] -> Right Nothing
    (e : _) -> Just <$> parseCaps2Info e
  return (mcaps1, mcaps2, rest2)

-- * Disco lookup via caps

{- | Resolve any caps the server advertises in stream features (XEP-0115 only,
as Caps 2.0 is not negotiated at the stream level by typical servers).
-}
streamFeatureCaps :: [Element] -> Maybe CapsInfo
streamFeatureCaps feats =
  let cs = filter (\e -> elementName e == capsName "c") feats
   in case mapMaybe (toRight . parseCapsInstance) cs of
        [] -> Nothing
        xs -> Just $ CapsInfo xs
 where
  toRight = either (const Nothing) Just

-- | Find caps published by the entity at @addr@ in its current presence.
presenceCaps :: forall m. (MonadStream m) => CapsPlugin m -> XMPPAddress -> m (Maybe CapsInfo, Maybe Caps2Info)
presenceCaps CapsPlugin {capsPluginPresence} addr
  | Just full <- fullJidGet addr = do
      m <- getAllPresences capsPluginPresence
      case M.lookup full m of
        Nothing -> return (Nothing, Nothing)
        Just ref ->
          let ext = presenceExtended (presenceValue ref)
           in return
                ( Reg.lookup (Proxy :: Proxy CapsInfo) ext
                , Reg.lookup (Proxy :: Proxy Caps2Info) ext
                )
  | otherwise = return (Nothing, Nothing)

{- | A peer-advertised caps variant we know how to validate. Carries the
resolved 'SomeXMPPHashAlgorithm' so neither 'validatedHashes' nor the
cache lookup needs to redo 'supportedHashAlgos' resolution.
-}
data CapsCandidate = CapsCandidate
  { ccVer :: CapsVer
  , ccNode :: Maybe Text
  {- ^ XEP-0115 @node@ attribute, if any (used to drive the disco IQ as
  @node='\<node\>#\<ver\>'@ per XEP-0115 §3.4). 'Nothing' for Caps 2.0.
  -}
  , ccAlgo :: HashAlgo
  , ccHash :: HashValue
  , ccHasher :: SomeXMPPHashAlgorithm
  }

{- | Build the list of supported candidates for the given caps tuple. Caps
2.0 candidates come first, then XEP-0115; within each XEP the order is
whatever 'CapsInfo' / 'Caps2Info' yields. Algorithms outside
'supportedHashAlgos' are dropped.
-}
candidateHashes :: Maybe CapsInfo -> Maybe Caps2Info -> [CapsCandidate]
candidateHashes mcaps1 mcaps2 =
  let mkCand ver node a v =
        (\some -> CapsCandidate {ccVer = ver, ccNode = node, ccAlgo = a, ccHash = v, ccHasher = some})
          <$> M.lookup a supportedHashAlgos
      caps2 =
        mapMaybe
          (\(a, v) -> mkCand Caps2 Nothing a v)
          [(a, v) | Caps2Info {caps2Hashes} <- maybeToList mcaps2, (a, v) <- M.toList caps2Hashes]
      caps1 =
        mapMaybe
          (\inst -> mkCand Caps1 (Just (capsNode inst)) (capsHash inst) (capsVer inst))
          [inst | CapsInfo {capsInstances} <- maybeToList mcaps1, inst <- capsInstances]
   in caps2 ++ caps1

{- | Validate that a fetched 'DiscoEntity' matches at least one of the caps
the entity had advertised. Each candidate is verified against the
verification string of the XEP under which it was advertised. The
verification strings are built at most once and reused across every
candidate of the same XEP.
-}
validatedHashes :: [CapsCandidate] -> DiscoEntity -> [(CapsVer, HashAlgo, HashValue)]
validatedHashes candidates entity =
  let bs1 = caps1Bytes entity
      bs2 = caps2Bytes entity
      bytesFor Caps1 = bs1
      bytesFor Caps2 = bs2
   in [ (ccVer c, ccAlgo c, ccHash c)
      | c <- candidates
      , ccHash c == hashLazyBase64 (ccHasher c) (bytesFor (ccVer c))
      ]

{- | Disco entity get-handler: serves disco lookups out of the persistent
caps cache when the target's advertised caps yield a cached entity.
On a miss, fetches via the plain IQ and on success — when the response
validates against at least one advertised hash — caches it.
-}
instance (MonadStream m) => Handler m (XMPPAddress, Maybe DiscoNode, Either StanzaError DiscoEntity -> m ()) () (CapsPlugin m) where
  tryHandle _ (_, Just _, _) = return Nothing -- caps cache only covers the bare entity, not nodes
  tryHandle cp@CapsPlugin {capsPluginCache, capsPluginDisco} (addr, Nothing, handler) = do
    let session = discoPluginSession capsPluginDisco
        isHome =
          addr == toXMPPAddress (ssBare session)
            || addr == toXMPPAddress (ssServer session)
        streamCaps =
          if isHome
            then streamFeatureCaps (sessionStreamFeatures (ssSession session))
            else Nothing
    (mc1, mc2) <- presenceCaps cp addr
    let candidates = candidateHashes (mc1 <|> streamCaps) mc2
    case candidates of
      [] -> return Nothing
      first : _ -> do
        cache <- readIORef capsPluginCache
        case listToMaybe [e | c <- candidates, Just e <- [M.lookup (ccVer c, ccAlgo c, ccHash c) (ccByHash cache)]] of
          Just entity -> Just <$> handler (Right entity)
          Nothing -> Just <$> fetchAndCache cp addr first candidates handler

{- | XEP-0115 §3.4 recommends issuing the disco IQ with @node='\<node\>#\<ver\>'@
to handle implementations that disambiguate by node. Caps 2.0 has no node,
so we fall back to a bare disco-info request.
-}
discoNodeForCandidate :: CapsCandidate -> Maybe DiscoNode
discoNodeForCandidate CapsCandidate {ccVer = Caps1, ccNode = Just node, ccHash = ver} =
  Just (node <> "#" <> ver)
discoNodeForCandidate _ = Nothing

fetchAndCache ::
  forall m.
  (MonadStream m) =>
  CapsPlugin m ->
  XMPPAddress ->
  CapsCandidate ->
  [CapsCandidate] ->
  (Either StanzaError DiscoEntity -> m ()) ->
  m ()
fetchAndCache CapsPlugin {capsPluginDisco, capsPluginCache} addr first candidates handler =
  requestDiscoEntity (discoPluginSession capsPluginDisco) addr (discoNodeForCandidate first) $ \resp -> do
    case resp of
      Right entity -> case validatedHashes candidates entity of
        [] -> $(logWarn) [i|Caps validation failed for #{addr}: no advertised hash matched the response|]
        matches -> atomicModifyIORef' capsPluginCache $ \c ->
          (c {ccByHash = foldr (`M.insert` entity) (ccByHash c) matches}, ())
      Left _ -> return ()
    handler resp

-- * Persistence

newtype CapsPluginCache m = CapsPluginCache (CapsPlugin m)

instance (MonadStream m) => XMPPPersistentCache m (CapsPluginCache m) where
  cacheKey _ = "caps"
  cacheGet (CapsPluginCache CapsPlugin {capsPluginCache}) = JSON.toJSON <$> readIORef capsPluginCache

-- * Plugin construction

getCapsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (CapsPlugin m)
getCapsPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (CapsPlugin m)) $ pluginsHooksSet pluginsRef

capsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
capsPlugin pluginsRef = do
  capsPluginDisco <- getDiscoPlugin pluginsRef
  capsPluginPresence <- getPresencePlugin pluginsRef
  let oldCache = pluginsOldCacheFor pluginsRef (Proxy :: Proxy (CapsPluginCache m)) >>= JSON.parseMaybe JSON.parseJSON
  capsPluginCache <- newIORef $ fromMaybe emptyCapsCache oldCache
  capsPluginOwnCaps <- newIORef Nothing
  let capsPluginNode = "https://github.com/abbradar/yaxmpp"
      algoNames1 = ["sha-1"]
      algoNames2 = ["sha-256"]
      resolveAlgo a = case M.lookup a supportedHashAlgos of
        Just some -> return some
        Nothing -> fail $ "capsPlugin: unsupported hash algorithm " <> show a
  capsPluginAlgos1 <- traverse resolveAlgo algoNames1
  capsPluginAlgos2 <- traverse resolveAlgo algoNames2
  let plugin :: CapsPlugin m = CapsPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  registerCacheGetter pluginsRef (CapsPluginCache plugin)
  Filter.pushNewOrFailM plugin (presencePluginFilters capsPluginPresence)
  HL.pushNewOrFailM plugin (discoPluginEntityGetHandlers capsPluginDisco)
