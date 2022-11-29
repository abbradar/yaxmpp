{-# LANGUAGE Strict #-}

module Network.XMPP.Stream
       ( ConnectionInterruptionException(..)
       , StreamErrorType(..)
       , StreamError(..)
       , unexpectedStanza
       , unexpectedInput
       , StreamSettings(..)
       , xmppVersion
       , streamSend
       , streamRecv
       , streamThrow
       , streamClose
       , streamKill
       , streamFeatures
       , streamInfo
       , streamIsClosed
       , ConnectionSettings(..)
       , ClientError(..)
       , MonadStream
       , Stream
       , streamCreate
       ) where

import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Exception (IOException)
import Control.Monad.IO.Unlift
import UnliftIO.IORef
import UnliftIO.MVar
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Catch
import Data.Conduit
import Data.Default.Class
import qualified Data.Conduit.List as CL
import Data.Conduit.ByteString.Builder
import Network.Connection
import Network.TLS (TLSException)
import Data.ByteString (ByteString)
import Text.XML
import Data.XML.Types (Event(..), Content(..))
import qualified Text.XML.Unresolved as XMLU
import qualified Text.XML.Stream.Parse as XMLP
import qualified Text.XML.Stream.Render as XMLR
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import qualified Data.ByteString.Base64 as B64
import Data.String.Interpolate (i)

import Data.Injective
import Network.SASL
import Network.XMPP.XML

tillFlush :: Monad m => ConduitT (Flush a) a m ()
tillFlush = await >>= \case
  Nothing -> return ()
  Just Flush -> return ()
  Just (Chunk a) -> yield a >> tillFlush

yieldTries :: Monad m => (forall o. ConduitT i o m (Maybe a)) -> ConduitT i a m ()
yieldTries comp = do
  mr <- comp
  case mr of
    Nothing -> return ()
    Just r -> do
      yield r
      yieldTries comp

maybeFlush :: Flush a -> Maybe a
maybeFlush (Chunk a) = Just a
maybeFlush Flush = Nothing

sourceConn :: (MonadIO m, MonadMask m) => Connection -> forall i. ConduitT i ByteString m ()
sourceConn conn = lift (handleConnErr $ connectionGetChunk conn) >>= \case
  "" -> return ()
  dat -> yield dat >> sourceConn conn

sinkConn :: (MonadIO m, MonadMask m) => Connection -> forall o. ConduitT ByteString o m ()
sinkConn conn = awaitForever $ lift . handleConnErr . connectionPut conn

data ConnectionInterruptionException = SocketException IOException
                                     | TLSException TLSException
                                     | ConnectionClosedException
                                     | OutStreamError StreamError
                                     | InStreamError StreamError
                                     deriving (Show, Typeable)

instance Exception ConnectionInterruptionException

handleConnErr :: (MonadIO m, MonadMask m) => IO a -> m a
handleConnErr = handle (throwM . TLSException) . handle (throwM . SocketException) . liftIO

data StreamErrorType = StBadFormat
                     | StBadNamespacePrefix
                     | StConflict
                     | StConnectionTimeout
                     | StHostGone
                     | StHostUnknown
                     | StImproperAddressing
                     | StInternalServerError
                     | StInvalidFrom
                     | StInvalidNamespace
                     | StInvalidXml
                     | StNotAuthorized
                     | StNotWellFormed
                     | StPolicyViolation
                     | StRemoteConnectionFailed
                     | StReset
                     | StResourceConstraint
                     | StRestrictedXml
                     | StSeeOtherHost
                     | StSystemShutdown
                     | StUndefinedCondition
                     | StUnsupportedEncoding
                     | StUnsupportedFeature
                     | StUnsupportedStanzaType
                     | StUnsupportedVersion
                     deriving (Show, Eq, Enum, Bounded, Ord)

instance Injective StreamErrorType Text where
  injTo x = case x of
    StBadFormat -> "bad-format"
    StBadNamespacePrefix -> "bad-namespace-prefix"
    StConflict -> "conflict"
    StConnectionTimeout -> "connection-timeout"
    StHostGone -> "host-gone"
    StHostUnknown -> "host-unknown"
    StImproperAddressing -> "improper-addressing"
    StInternalServerError -> "internal-server-error"
    StInvalidFrom -> "invalid-from"
    StInvalidNamespace -> "invalid-namespace"
    StInvalidXml -> "invalid-xml"
    StNotAuthorized -> "not-authorized"
    StNotWellFormed -> "not-well-formed"
    StPolicyViolation -> "policy-violation"
    StRemoteConnectionFailed -> "remote-connection-failed"
    StReset -> "reset"
    StResourceConstraint -> "resource-constraint"
    StRestrictedXml -> "restricted-xml"
    StSeeOtherHost -> "see-other-host"
    StSystemShutdown -> "system-shutdown"
    StUndefinedCondition -> "undefined-condition"
    StUnsupportedEncoding -> "unsupported-encoding"
    StUnsupportedFeature -> "unsupported-feature"
    StUnsupportedStanzaType -> "unsupported-stanza-type"
    StUnsupportedVersion -> "unsupported-version"

data StreamError = StreamError { smeType :: StreamErrorType
                               , smeText :: Maybe Text
                               , smeChildren :: [Element]
                               }
                 deriving (Show, Eq)

undefinedError :: StreamError
undefinedError = StreamError { smeType = StUndefinedCondition
                             , smeText = Nothing
                             , smeChildren = []
                             }

intersperse :: (Foldable t, Monoid s) => s -> t s -> s
intersperse sep = foldr1 (\a b -> a <> sep <> b)

unexpectedStanza :: Name -> [Name] -> StreamError
unexpectedStanza got expected =
  undefinedError { smeType = StInvalidXml
                 , smeText = Just $ [i|Unexpected stanza: got <#{nameLocalName got}>, expected one of |] <>
                             intersperse ", " (map (\n -> [i|<#{nameLocalName n}>|]) expected)
                 }

unexpectedInput :: Text -> StreamError
unexpectedInput str =
  undefinedError { smeType = StInvalidXml
                 , smeText = Just [i|Stanza error: #{str}|]
                 }

xmlError :: XMLException -> StreamError
xmlError e =
  undefinedError { smeType = StInvalidXml
                 , smeText = Just [i|XML parse error: #{e}|]
                 }

unresolvedEntity :: Set Text -> StreamError
unresolvedEntity entities =
  undefinedError { smeType = StInvalidXml
                 , smeText = Just $ "Unresolved XML entities: " <>
                             intersperse ", " (S.toList entities)
                 }

unsupportedVersion :: Text -> StreamError
unsupportedVersion ver =
  undefinedError { smeType = StUnsupportedVersion
                 , smeText = Just [i|Supported version: #{xmppVersion}, got #{ver}|]
                 }


data StreamSettings = StreamSettings { ssFrom :: Text
                                     , ssId :: Text
                                     , ssVersion :: Text
                                     , ssLang :: Text
                                     }
                     deriving (Show, Eq)

xmppVersion :: Text
xmppVersion = "1.0"

streamOutTag :: Name
streamOutTag = Name "stream" (Just streamNS) (Just "stream")

startEvents :: Text -> Text -> [Event]
startEvents from to =
  [ EventBeginDocument
  , EventBeginElement streamOutTag
    [ (xmlName "lang", [ContentText "en"])

    , ("version", [ContentText xmppVersion])
    , ("from", [ContentText from])
    , ("to", [ContentText to])
    -- Needed by ejabberd >= 16.12
    , ("xmlns", [ContentText jcNS])
    ]
  ]

stopEvents :: [Event]
stopEvents =
  [ EventEndElement streamOutTag
  , EventEndDocument
  ]

streamTag :: MonadThrow m => (StreamSettings -> ConduitM Event o m c) -> ConduitM Event o m c
streamTag = XMLP.force "Stream has not started" . XMLP.tag' (XMLP.matching (== streamName "stream")) streamAttrs
  where streamAttrs = do
          ssLang <- XMLP.requireAttr $ xmlName "lang"
          ssId <- XMLP.requireAttr "id"
          ssFrom <- XMLP.requireAttr "from"
          ssVersion <- XMLP.requireAttr "version"
          return StreamSettings {..}

streamNS :: Text
streamName :: Text -> Name
(streamNS, streamName) = namePair "http://etherx.jabber.org/streams"

startTLSName :: Text -> Name
startTLSName = nsName "urn:ietf:params:xml:ns:xmpp-tls"

saslName :: Text -> Name
saslName = nsName "urn:ietf:params:xml:ns:xmpp-sasl"

estreamName :: T.Text -> Name
estreamName = nsName "urn:ietf:params:xml:ns:xmpp-streams"

data StartTLSFeature = StartTLSFeature { tlsMandatory :: Bool }
                     deriving (Show, Eq)

parseStartTLS :: Element -> Maybe StartTLSFeature
parseStartTLS e | elementName e == startTLSName "starttls" = Just $ StartTLSFeature $ not $ null $ fromElement e $/ XC.element (startTLSName "required")
                | otherwise = Nothing

data SASLFeature = SASLFeature { saslMechanisms :: [ByteString] }
                 deriving (Show, Eq)

parseSASL :: Element -> Maybe SASLFeature
parseSASL e | elementName e == saslName "mechanisms" = Just $ SASLFeature $ map T.encodeUtf8 $ fromElement e $/ XC.element (saslName "mechanism") &/ content
            | otherwise = Nothing

type MonadStream m = (MonadMask m, MonadLogger m, MonadUnliftIO m, MonadFail m)

data ConnectionSettings = ConnectionSettings { connectionParams :: ConnectionParams
                                             , connectionContext :: ConnectionContext
                                             , connectionServer :: Text
                                             , connectionUser :: Text
                                             , connectionAuth :: [SASLAuthenticator ()]
                                             }

renderStanza :: MonadStream m => Text -> Text -> ConduitT Element (Flush Event) m ()
renderStanza from to = do
  mapM_ (yield . Chunk) $ startEvents from to
  yield Flush
  awaitForever $ \e -> do
    $(logDebug) [i|Sending message: #{showElement e}|]
    mapM_ (yield . Chunk) $ XMLU.elementToEvents $ toXMLElement e
    yield Flush
  mapM_ (yield . Chunk) stopEvents

-- "Sealed conduit" for rendering one element at a time.
-- This requires several invariants to hold:
-- 1) 'renderStanza' first yields several Events and then Flush, without getting an Element.
-- 2) 'renderStanza' then yields several Events and Flush per each Element that it gets.
createStreamRender :: MonadStream m => ConnectionSettings -> Connection -> m (SealedConduitT Element (Flush ByteString) m ())
createStreamRender (ConnectionSettings {..}) conn = do
  let render = sealConduitT $
               renderStanza [i|#{connectionUser}@#{connectionServer}|] connectionServer
               .| XMLR.renderBuilderFlush renderSettings
               .| transPipe liftIO builderToByteStringFlush
  (render', _) <- runConduit $ CL.sourceNull .| (render =$$++ tillFlush .| sinkConn conn)
  return render'

  where renderSettings = def { rsNamespaces = [ ("stream", streamNS)
                                              , ("xml", xmlNS)
                                              ]
                             }

data InternalStreamException = InternalStreamException StreamError
                             deriving (Show, Typeable)

instance Exception InternalStreamException

parseStanza :: MonadStream m => IORef (Maybe StreamSettings) -> ConduitT Event Element m ()
parseStanza streamcfgR = handleC (throwM . InternalStreamException . xmlError) $ streamTag $ \streamcfg -> do
  writeIORef streamcfgR $ Just streamcfg
  fuseLeftovers (map snd) (CL.map (Nothing, )) (yieldTries XMLU.elementFromEvents) .| CL.mapM tryFromElement

  where tryFromElement e' = case fromXMLElement e' of
          Right e -> do
            $(logDebug) [i|Received message: #{showElement e}|]
            return e
          Left unres -> throwM $ InternalStreamException $ unresolvedEntity unres

getStreamError :: Element -> Maybe StreamError
getStreamError e
  | elementName e == estreamName "error" =
      Just StreamError {..}
  | otherwise = Nothing

  where cur = fromElement e

        smeType = fromMaybe StUndefinedCondition $ do
          en <- listToMaybe $ cur $/ curAnyElement
          injFrom $ nameLocalName $ elementName en

        smeText = listToMaybe $ cur $/ XC.element (estreamName "text") &/ content

        smeChildren = map (\(node -> NodeElement ec) -> ec) $ cur $/ checkName (\n -> n /= estreamName (injTo smeType) && n /= estreamName "text")

createStreamSource :: MonadStream m => Connection -> m (StreamSettings, SealedConduitT () Element m ())
createStreamSource conn = do
  streamcfgR <- newIORef Nothing
  let checkError = CL.mapM $ \e -> case getStreamError e of
        Nothing -> return e
        Just err -> throwM $ InStreamError err
      source = sealConduitT $
               sourceConn conn
               .| XMLP.parseBytes parseSettings
               .| parseStanza streamcfgR
               .| checkError
  (source', _) <- source $$++ CL.peek
  Just streamcfg <- readIORef streamcfgR
  return (streamcfg, source')
  
  where parseSettings = def

data Stream m = Stream { streamConn :: Connection
                       , streamSource :: MVar (SealedConduitT () Element m ())
                       , streamRender :: MVar (SealedConduitT Element (Flush ByteString) m ())
                       , streamFeatures :: [Element]
                       , streamInfo :: StreamSettings
                       , streamClosedFlag :: IORef Bool
                       }

streamSend :: MonadStream m => Stream m -> Element -> m ()
streamSend (Stream {..}) msg = modifyMVar streamRender $ \render ->
  runConduit $ yield msg .| (render =$$++ tillFlush .| sinkConn streamConn)

streamRecv' :: MonadStream m => Stream m -> m (Maybe Element)
streamRecv' stream@(Stream {..}) = modifyMVar streamSource $ \source ->
  handle internalError (source $$++ CL.head)

  where internalError (InternalStreamException e) = streamThrow stream e

streamRecv :: MonadStream m => Stream m -> m Element
streamRecv stream = streamRecv' stream >>= \case
  Nothing -> throwM ConnectionClosedException
  Just msg -> return msg

streamThrow :: MonadStream m => Stream m -> StreamError -> m a
streamThrow stream e@(StreamError {..}) = do
  streamSend stream $ element (streamName "error") [] $
    [ NodeElement $ closedElement $ estreamName $ injTo smeType
    ]
    ++ maybeToList (fmap (\t -> NodeElement $ element (estreamName "text") [(xmlName "lang", "en")] [NodeContent t]) smeText)
    ++ map NodeElement smeChildren
  throwM $ OutStreamError e

streamClose :: MonadStream m => Stream m -> m ()
streamClose (Stream {..}) = do
  $(logInfo) "Closing stream"
  modifyMVar_ streamRender $ \render -> do
    closed <- readIORef streamClosedFlag
    unless closed $ do
      runConduit $ CL.sourceNull .| (render =$$+- CL.mapMaybe maybeFlush .| sinkConn streamConn)
      atomicWriteIORef streamClosedFlag True
    return $ sealConduitT $ throwM ConnectionClosedException

streamKill :: MonadStream m => Stream m -> m ()
streamKill (Stream {..}) = liftIO $ connectionClose streamConn

streamIsClosed :: MonadStream m => Stream m -> m Bool
streamIsClosed (Stream {..}) = readIORef streamClosedFlag

saslAuth :: MonadStream m => Stream m -> [SASLAuthenticator r] -> m (Maybe r)
saslAuth _ [] = return Nothing
saslAuth s (auth:others) = do
  $(logInfo) [i|Trying auth method #{T.decodeUtf8 (saslMechanism auth)}|]
  streamSend s $ element (saslName "auth") [("mechanism", T.decodeUtf8 $ saslMechanism auth)] $
    maybeToList $ fmap (NodeContent . T.decodeUtf8 . B64.encode) $ saslInitial auth
  proceedAuth $ saslWire auth

  where proceedAuth wire = do
          eresp <- streamRecv s
          let mdat = listToMaybe $ fmap (B64.decode . T.encodeUtf8) $ fromElement eresp $/ content
          mdat' <- forM mdat $ \case
            Left err -> streamThrow s $ unexpectedInput [i|proceedAuth, base64 decode: #{err}|]
            Right d -> return d
          resp <- case mdat' of
            d | elementName eresp == saslName "success" -> return $ SASLSuccess d
            Nothing | elementName eresp == saslName "failure" -> return $ SASLFailure
            Just dat | elementName eresp == saslName "challenge" -> return $ SASLChallenge dat
            _ -> streamThrow s $ unexpectedStanza (elementName eresp) $ map saslName ["challenge", "success", "failure"]
          res <- liftIO $ runSASLWire wire resp
          case res of
            Left (msg, wire') -> do
              streamSend s $ case msg of
                SASLResponse r -> element (saslName "response") [] [NodeContent $ T.decodeUtf8 $ B64.encode r]
                SASLAbort -> closedElement (saslName "abort")
              proceedAuth wire'
            Right (Just a) -> do
              $(logInfo) "Successfully authenticated"
              return $ Just a
            Right Nothing -> saslAuth s others

data FeaturesResult = FeaturesOK
                    | FeaturesUnauthorized
                    | FeaturesRestart
                    | FeaturesTLS
                    deriving (Show, Eq)

initFeatures :: MonadStream m => ConnectionSettings -> Stream m -> [Element] -> m FeaturesResult
initFeatures (ConnectionSettings {..}) s features
  | StartTLSFeature {}:_ <- mapMaybe parseStartTLS features = do
      $(logInfo) "Negotiating TLS"
      streamSend s $ closedElement $ startTLSName "starttls"
      eanswer <- streamRecv s
      unless (elementName eanswer == startTLSName "proceed") $ streamThrow s $ unexpectedStanza (elementName eanswer) [startTLSName "proceed"]
      return FeaturesTLS
  
  | SASLFeature methods:_ <- mapMaybe parseSASL features = do
      $(logInfo) "Authenticating"
      r <- saslAuth s $ concatMap (\m -> filter (\a -> saslMechanism a == m) connectionAuth) methods
      return $ if isJust r then FeaturesRestart else FeaturesUnauthorized

  | otherwise = return FeaturesOK

data ClientError = Unauthorized
                 | NoTLSParams
                 deriving (Show, Eq)

parseFeatures :: MonadStream m => Stream m -> Element -> m [Element]
parseFeatures stream e
  | elementName e == streamName "features" = return $ fromElement e $/ anyElement &| curElement
  | otherwise = streamThrow stream $ unexpectedStanza (elementName e) [streamName "features"]

streamCreate :: MonadStream m => ConnectionSettings -> m (Either ClientError (Stream m))
streamCreate csettings@(ConnectionSettings {..}) =
  bracketOnError (handleConnErr $ connectTo connectionContext connectionParams { connectionUseSecure = Nothing }) (liftIO . connectionClose) initStream

  where
    initStream streamConn = do
      $(logInfo) "Initializing stream"

      streamRender' <- createStreamRender csettings streamConn
      (streamInfo, streamSource') <- createStreamSource streamConn
      streamRender <- newMVar streamRender'
      streamSource <- newMVar streamSource'
      streamClosedFlag <- newIORef False

      let stream = Stream { streamFeatures = []
                          , ..
                          }

      (features, r) <- do
        efeatures <- streamRecv stream
        if ssVersion streamInfo /= xmppVersion
          then streamThrow stream $ unsupportedVersion $ ssVersion streamInfo
          else do
            features <- parseFeatures stream efeatures
            r <- initFeatures csettings stream features
            return (features, r)

      case r of
        FeaturesUnauthorized -> do
          streamClose stream
          return $ Left Unauthorized

        FeaturesOK -> return $ Right stream { streamFeatures = features }

        FeaturesRestart -> initStream streamConn

        FeaturesTLS -> case connectionUseSecure connectionParams of
          Nothing -> return $ Left NoTLSParams
          Just tls -> do
            handleConnErr $ connectionSetSecure connectionContext streamConn tls
            initStream streamConn
