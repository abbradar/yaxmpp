module Network.XMPP.Stream
       ( ConnectionInterruptionException(..)
       , StreamErrorType(..)
       , StreamError(..)
       , unexpectedStanza
       , unexpectedInput
       , StreamSettings(..)
       , xmppVersion
       , StreamFeature(..)
       , streamSend
       , streamRecv
       , streamThrow
       , streamClose
       , streamFeatures
       , streamInfo
       , ConnectionSettings(..)
       , ClientError(..)
       , MonadStream
       , Stream
       , streamCreate
       ) where

import Data.Monoid
import Data.Maybe
import Data.List (find)
import Data.Typeable
import Control.Monad
import Control.Exception (IOException)
import Data.IORef.Lifted
import Control.Concurrent.MVar.Lifted
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Catch
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.ByteString.Builder
import Network.Connection
import Network.TLS (TLSException)
import Data.ByteString (ByteString)
import Data.Default
import Text.XML
import Data.XML.Types (Event(..), Content(..))
import qualified Text.XML.Unresolved as XMLU
import qualified Text.XML.Stream.Parse as XMLP
import qualified Text.XML.Stream.Render as XMLR
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import qualified Data.ByteString.Base64 as B64
import Text.InterpolatedString.Perl6 (qq)

import Data.Injective
import Network.SASL
import Network.XMPP.XML

tillFlush :: Monad m => Conduit (Flush a) m a
tillFlush = await >>= \case
  Nothing -> return ()
  Just Flush -> return ()
  Just (Chunk a) -> yield a >> tillFlush

yieldTries :: Monad m => Consumer i m (Maybe a) -> Conduit i m a
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

sourceConn :: (MonadIO m, MonadMask m) => Connection -> Producer m ByteString
sourceConn conn = lift (handleConnErr $ connectionGetChunk conn) >>= \case
  "" -> return ()
  dat -> yield dat >> sourceConn conn

sinkConn :: (MonadIO m, MonadMask m) => Connection -> Consumer ByteString m ()
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

intersperse :: (Foldable t, Monoid s) => s -> t s -> s
intersperse sep = foldr1 (\a b -> a <> sep <> b)

unexpectedStanza :: Name -> [Name] -> StreamError
unexpectedStanza got expected =
  StreamError { smeType = StInvalidXml
              , smeText = Just $ [qq|Unexpected stanza: got <{nameLocalName got}>, expected one of |] <>
                         intersperse ", " (map (\n -> [qq|<{nameLocalName n}>|]) expected)
              , smeChildren = []
              }

unexpectedInput :: Text -> StreamError
unexpectedInput str =
  StreamError { smeType = StInvalidXml
              , smeText = Just [qq|Stanza error: $str|]
              , smeChildren = []
              }

xmlError :: XMLException -> StreamError
xmlError e =
  StreamError { smeType = StInvalidXml
              , smeText = Just [qq|XML parse error: $e|]
              , smeChildren = []
              }

unresolvedEntity :: Set Text -> StreamError
unresolvedEntity entities =
  StreamError { smeType = StInvalidXml
              , smeText = Just $ "Unresolved XML entities: " <>
                         intersperse ", " (S.toList entities)
              , smeChildren = []
              }

unsupportedVersion :: Text -> StreamError
unsupportedVersion ver =
  StreamError { smeType = StUnsupportedVersion
              , smeText = Just [qq|Supported version: $xmppVersion, got $ver|]
              , smeChildren = []
              }


data StreamSettings = StreamSettings { ssFrom :: Text
                                     , ssId :: Text
                                     , ssVersion :: Text
                                     , ssLang :: Text
                                     }
                     deriving (Show, Eq)

xmppVersion :: Text
xmppVersion = "1.0"

startEvents :: Text -> Text -> [Event]
startEvents from to =
  [ EventBeginDocument
  , EventBeginElement (streamName "stream")
    [ (xmlName "lang", [ContentText "en"])

    , ("version", [ContentText xmppVersion])
    , ("from", [ContentText from])
    , ("to", [ContentText to])
    , ("xmlns", [ContentText jcNS])
    ]
  ]

stopEvents :: [Event]
stopEvents =
  [ EventEndElement (streamName "stream")
  , EventEndDocument
  ]

streamTag :: MonadThrow m => (StreamSettings -> ConduitM Event o m c) -> ConduitM Event o m c
streamTag = XMLP.force "Stream has not started" . XMLP.tagName (streamName "stream") streamAttrs
  where streamAttrs = do
          ssLang <- XMLP.requireAttr $ xmlName "lang"
          ssId <- XMLP.requireAttr "id"
          ssFrom <- XMLP.requireAttr "from"
          ssVersion <- XMLP.requireAttr "version"
          return StreamSettings {..}

streamNS :: Text
streamName :: Text -> Name
(streamNS, streamName) = namePair "http://etherx.jabber.org/streams"

compressionName :: Text -> Name
compressionName = nsName "http://jabber.org/features/compress"

startTLSName :: Text -> Name
startTLSName = nsName "urn:ietf:params:xml:ns:xmpp-tls"

saslName :: Text -> Name
saslName = nsName "urn:ietf:params:xml:ns:xmpp-sasl"

estreamName :: T.Text -> Name
estreamName = nsName "urn:ietf:params:xml:ns:xmpp-streams"

data StreamFeature = Compression [Text]
                   | StartTLS Bool
                   | SASL [ByteString]
                   | StreamManagement
                   | BindResource
                   | RosterVersioning
                   deriving (Show, Eq)

type MonadStream m = (MonadIO m, MonadMask m, MonadLogger m, MonadBaseControl IO m)

parseFeatures :: MonadStream m => Stream m -> Element -> m [StreamFeature]
parseFeatures stream e
  | elementName e == streamName "features" = catMaybes <$> mapM parseOne (elementNodes e)
  | otherwise = streamThrow stream $ unexpectedStanza (elementName e) [streamName "features"]

  where parseOne n@(NodeElement f)
          | elementName f == startTLSName "starttls" =
              return $ Just $ StartTLS $ not $ null $ fromNode n $/ XC.element (startTLSName "required")
          | elementName f == compressionName "compression" =
              return $ Just $ Compression $ fromNode n $/ XC.element (compressionName "method") &/ content
          | elementName f == saslName "mechanisms" =
              return $ Just $ SASL $ map T.encodeUtf8 $ fromNode n $/ XC.element (saslName "mechanism") &/ content
          | elementName f == smName "sm" = return $ Just StreamManagement
          | elementName f == bindName "bind" = return $ Just BindResource
          | elementName f == rosterVerName "ver" = return $ Just RosterVersioning
          | otherwise = do
              $(logWarn) [qq|"Unknown feature: {showElement f}|]
              return Nothing

        parseOne _ = return Nothing

data ConnectionSettings = ConnectionSettings { connectionParams :: ConnectionParams
                                             , connectionContext :: ConnectionContext
                                             , connectionServer :: Text
                                             , connectionUser :: Text
                                             , connectionAuth :: [SASLAuthenticator ()]
                                             }

renderStanza :: MonadStream m => Text -> Text -> Conduit Element m (Flush Event)
renderStanza from to = do
  mapM_ (yield . Chunk) $ startEvents from to
  yield Flush
  awaitForever $ \e -> do
    $(logDebug) $ "Sending message: " <> showElement e
    mapM_ (yield . Chunk) $ XMLU.elementToEvents $ toXMLElement e
    yield Flush
  mapM_ (yield . Chunk) stopEvents

-- "Resumable conduit" for rendering one element at a time.
-- This requires several invariants to hold:
-- 1) 'renderStanza' first yields several Events and then Flush, without getting an Element.
-- 2) 'renderStanza' then yields several Events and Flush per each Element that it gets.
createStreamRender :: MonadStream m => ConnectionSettings -> Connection -> m (ResumableConduit Element m (Flush ByteString))
createStreamRender (ConnectionSettings {..}) conn = do
  let render = newResumableConduit $
               renderStanza [qq|{connectionUser}@{connectionServer}|] connectionServer
               =$= XMLR.renderBuilderFlush renderSettings
               =$= builderToByteStringFlush
  (render', _) <- CL.sourceNull $$ render =$$++ tillFlush =$= sinkConn conn
  return render'

  where renderSettings = def { rsNamespaces = [ ("stream", streamNS)
                                              , ("xml", xmlNS)
                                              ]
                             }

data InternalStreamException = InternalStreamException StreamError
                             deriving (Show, Typeable)

instance Exception InternalStreamException

parseStanza :: MonadStream m => IORef (Maybe StreamSettings) -> Conduit Event m Element
parseStanza streamcfgR = handle (throwM . InternalStreamException . xmlError) $ streamTag $ \streamcfg -> do
  writeIORef streamcfgR $ Just streamcfg
  fuseLeftovers (map snd) (CL.map (Nothing, )) (yieldTries XMLU.elementFromEvents) =$= CL.mapM tryFromElement

  where tryFromElement e' = case fromXMLElement e' of
          Right e -> do
            $(logDebug) [qq|Received message: {showElement e}|]
            return e
          Left unres -> throwM $ InternalStreamException $ unresolvedEntity unres

getStreamError :: Element -> Maybe StreamError
getStreamError e
  | elementName e == estreamName "error" =
      Just StreamError {..}
  | otherwise = Nothing

  where cur = fromNode $ NodeElement e

        smeType = fromMaybe StUndefinedCondition $ do
          NodeElement en <- listToMaybe $ cur $/ anyElement &| node
          injFrom $ nameLocalName $ elementName en

        smeText = listToMaybe $ cur $/ XC.element (estreamName "text") &/ content

        smeChildren = map (\(node -> NodeElement ec) -> ec) $ cur $/ checkName (\n -> n /= estreamName (injTo smeType) && n /= estreamName "text")

createStreamSource :: MonadStream m => Connection -> m (StreamSettings, ResumableSource m Element)
createStreamSource conn = do
  streamcfgR <- newIORef Nothing
  let checkError = CL.mapM $ \e -> case getStreamError e of
        Nothing -> return e
        Just err -> throwM $ InStreamError err
      source = newResumableSource $
               sourceConn conn
               =$= XMLP.parseBytes parseSettings
               =$= parseStanza streamcfgR
               =$= checkError
  (source', _) <- source $$++ CL.peek
  Just streamcfg <- readIORef streamcfgR
  return (streamcfg, source')
  
  where parseSettings = def

data Stream m = Stream { streamConn :: Connection
                       , streamSource :: MVar (ResumableSource m Element)
                       , streamRender :: MVar (ResumableConduit Element m (Flush ByteString))
                       , streamFeatures :: [StreamFeature]
                       , streamInfo :: StreamSettings
                       }

streamSend :: MonadStream m => Stream m -> Element -> m ()
streamSend (Stream {..}) msg = modifyMVar streamRender $ \render ->
  yield msg $$ render =$$++ tillFlush =$= sinkConn streamConn

streamRecv' :: MonadStream m => Stream m -> m (Maybe Element)
streamRecv' stream@(Stream {..}) = modifyMVar streamSource $ \source ->
  handle internalError (source $$++ CL.head)

  where internalError (InternalStreamException e) = streamThrow stream e

streamRecv :: MonadStream m => Stream m -> m Element
streamRecv s = streamRecv' s >>= \case
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
  $(logInfo) "Closing connection"
  render <- takeMVar streamRender
  CL.sourceNull $$ render =$$+- CL.mapMaybe maybeFlush =$= sinkConn streamConn
  source <- takeMVar streamSource
  source $$+- CL.sinkNull
  liftIO $ connectionClose streamConn

saslAuth :: MonadStream m => Stream m -> [SASLAuthenticator r] -> m (Maybe r)
saslAuth _ [] = return Nothing
saslAuth s (auth:others) = do
  $(logInfo) [qq|Trying auth method {T.decodeUtf8 (saslMechanism auth)}|]
  streamSend s $ element (saslName "auth") [("mechanism", T.decodeUtf8 $ saslMechanism auth)] $
    maybeToList $ fmap (NodeContent . T.decodeUtf8 . B64.encode) $ saslInitial auth
  proceedAuth $ saslWire auth

  where proceedAuth wire = do
          eresp <- streamRecv s
          let mdat = listToMaybe $ fmap (B64.decode . T.encodeUtf8) $ fromNode (NodeElement eresp) $/ content
          mdat' <- forM mdat $ \case
            Left err -> streamThrow s $ unexpectedInput [qq|proceedAuth, base64 decode: $err|]
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

initFeatures :: MonadStream m => ConnectionSettings -> Stream m -> [StreamFeature] -> m FeaturesResult
initFeatures (ConnectionSettings {..}) s features
  | any (\case StartTLS _ -> True; _ -> False) features = do
      $(logInfo) "Negotiating TLS"
      streamSend s $ closedElement $ startTLSName "starttls"
      eanswer <- streamRecv s
      unless (elementName eanswer == startTLSName "proceed") $ streamThrow s $ unexpectedStanza (elementName eanswer) [startTLSName "proceed"]
      return FeaturesTLS
  
  | Just (SASL methods) <- find (\case SASL _ -> True; _ -> False) features = do
      $(logInfo) "Authenticating"
      r <- saslAuth s $ concatMap (\m -> filter (\a -> saslMechanism a == m) connectionAuth) methods
      return $ if isJust r then FeaturesRestart else FeaturesUnauthorized

  | otherwise = return FeaturesOK

data ClientError = Unauthorized
                 | NoTLSParams
                 deriving (Show, Eq)

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
