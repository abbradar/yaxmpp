module Network.XMPP.Stream
       ( ConnectionInterruptionException(..)
       , StreamSettings(..)
       , xmppVersion
       , StreamException(..)
       , streamErrorMsg
       , StreamFeature(..)
       , Stream
       , streamSend
       , streamRecv'
       , streamRecv
       , streamClose
       , streamFeatures
       , streamInfo
       , ConnectionSettings(..)
       , ClientError(..)
       , MonadStream
       , createStream
       ) where

import Data.Monoid
import Data.Maybe
import Data.Typeable
import Data.IORef.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Exception (IOException)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.ByteString.Builder
import Network.Connection
import Network.TLS (TLSException)
import Data.Set (Set)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Default
import Text.XML
import Data.XML.Types (Event(..), Content(..))
import qualified Text.XML.Unresolved as XMLU
import qualified Text.XML.Stream.Parse as XMLP
import qualified Text.XML.Stream.Render as XMLR
import Text.XML.Cursor hiding (element)
import qualified Data.ByteString.Base64 as B64

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

data ConnectionInterruptionException = SocketException IOException
                                     | TLSException TLSException
                                     | ConnectionClosedException
                                     deriving (Show, Typeable)
instance Exception ConnectionInterruptionException

handleConnErr :: (MonadIO m, MonadMask m) => IO a -> m a
handleConnErr = handle (throwM . TLSException) . handle (throwM . SocketException) . liftIO

sourceConn :: (MonadIO m, MonadMask m) => Connection -> Producer m ByteString
sourceConn conn = lift (handleConnErr $ connectionGetChunk conn) >>= \case
  "" -> return ()
  dat -> yield dat >> sourceConn conn

sinkConn :: (MonadIO m, MonadMask m) => Connection -> Consumer ByteString m ()
sinkConn conn = awaitForever $ lift . handleConnErr . connectionPut conn

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

compressionName :: Text -> Name
compressionName = nsName "http://jabber.org/features/compress"

startTLSName :: Text -> Name
startTLSName = nsName "urn:ietf:params:xml:ns:xmpp-tls"

saslName :: Text -> Name
saslName = nsName "urn:ietf:params:xml:ns:xmpp-sasl"

estreamName :: T.Text -> Name
estreamName = nsName "urn:ietf:params:xml:ns:xmpp-streams"

data StreamException = UnexpectedStanza Name [Name]
                     | XMLError XMLP.XmlException
                     | UnresolvedEntity (Set Text)
                     | UnsupportedVersion Text
                     | UnexpectedInput String
                     deriving (Show, Typeable)

instance Exception StreamException

streamErrorMsg :: StreamException -> Element
streamErrorMsg (UnexpectedStanza _ _) = closedElement $ estreamName "invalid-xml"
streamErrorMsg (XMLError _) = closedElement $ estreamName "not-well-formed"
streamErrorMsg (UnresolvedEntity _) = closedElement $ estreamName "not-well-formed"
streamErrorMsg (UnsupportedVersion _) = closedElement $ estreamName "unsupported-version"
streamErrorMsg (UnexpectedInput _) = closedElement $ estreamName "invalid-xml"

data StreamFeature = Compression [Text]
                   | StartTLS Bool
                   | SASL [ByteString]
                   | StreamManagement
                   | BindResource
                   deriving (Show, Eq)

type MonadStream m = (MonadIO m, MonadMask m, MonadLogger m, MonadBase IO m)

parseFeatures :: MonadStream m => Element -> m [StreamFeature]
parseFeatures e
  | elementName e == streamName "features" = catMaybes <$> mapM parseOne (elementNodes e)
  | otherwise = throwM $ UnexpectedStanza (elementName e) [streamName "features"]

  where parseOne n@(NodeElement f)
          | elementName f == startTLSName "starttls" =
              return $ Just $ StartTLS $ not $ null $ fromNode n $/ checkName (== startTLSName "required")
          | elementName f == compressionName "compression" =
              return $ Just $ Compression $ fromNode n $/ checkName (== compressionName "method") &/ content
          | elementName f == saslName "mechanisms" =
              return $ Just $ SASL $ map T.encodeUtf8 $ fromNode n $/ checkName (== saslName "mechanism") &/ content
          | elementName f == smName "sm" = return $ Just StreamManagement
          | elementName f == bindName "bind" = return $ Just BindResource
          | otherwise = do
              $(logWarn) $ "Unknown feature: " <> showElement f
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
               renderStanza (connectionUser <> "@" <> connectionServer) connectionServer
               =$= XMLR.renderBuilderFlush renderSettings
               =$= builderToByteStringFlush
  (render', _) <- CL.sourceNull $$ render =$$++ tillFlush =$= sinkConn conn
  return render'

  where renderSettings = def { rsNamespaces = [ ("stream", streamNS)
                                              , ("xml", xmlNS)
                                              ]
                             }

parseStanza :: MonadStream m => IORef (Maybe StreamSettings) -> Conduit Event m Element
parseStanza streamcfgR = handle (throwM . XMLError) $ streamTag $ \streamcfg -> do
  writeIORef streamcfgR $ Just streamcfg
  fuseLeftovers (map snd) (CL.map (Nothing, )) (yieldTries XMLU.elementFromEvents) =$= CL.mapM tryFromElement

  where tryFromElement e' = case fromXMLElement e' of
          Right e -> do
            $(logDebug) $ "Received message: " <> showElement e
            return e
          Left unres -> throwM $ UnresolvedEntity unres

createStreamSource :: MonadStream m => Connection -> m (StreamSettings, ResumableSource m Element)
createStreamSource conn = do
  streamcfgR <- newIORef Nothing
  let source = newResumableSource $
               sourceConn conn
               =$= XMLP.parseBytes parseSettings
               =$= parseStanza streamcfgR
  (source', _) <- source $$++ CL.peek
  Just streamcfg <- readIORef streamcfgR
  return (streamcfg, source')
  
  where parseSettings = def

data Stream m = Stream { streamConn :: Connection
                       , streamSource :: IORef (ResumableSource m Element)
                       , streamRender :: IORef (ResumableConduit Element m (Flush ByteString))
                       , streamFeatures :: [StreamFeature]
                       , streamInfo :: StreamSettings
                       }

streamSend :: MonadStream m => Stream m -> Element -> m ()
streamSend (Stream {..}) msg = do
  render <- readIORef streamRender
  (render', _) <- yield msg $$ render =$$++ tillFlush =$= sinkConn streamConn
  writeIORef streamRender render'

streamRecv' :: MonadStream m => Stream m -> m (Maybe Element)
streamRecv' (Stream {..}) = do
  source <- readIORef streamSource
  (source', mmsg) <- source $$++ CL.head
  writeIORef streamSource source'
  return mmsg

streamRecv :: MonadStream m => Stream m -> m Element
streamRecv s = streamRecv' s >>= \case
  Nothing -> throwM ConnectionClosedException
  Just msg -> return msg

streamClose :: MonadStream m => Stream m -> m ()
streamClose (Stream {..}) = do
  $(logInfo) "Closing connection"
  render <- readIORef streamRender
  CL.sourceNull $$ render =$$+- CL.mapMaybe maybeFlush =$= sinkConn streamConn
  source <- readIORef streamSource
  source $$+- CL.sinkNull
  liftIO $ connectionClose streamConn

saslAuth :: MonadStream m => Stream m -> [SASLAuthenticator r] -> m (Maybe r)
saslAuth _ [] = return Nothing
saslAuth s (auth:others) = do
  $(logInfo) $ "Trying auth method " <> T.decodeUtf8 (saslMechanism auth)
  streamSend s $ element (saslName "auth") [("mechanism", T.decodeUtf8 $ saslMechanism auth)] $
    maybeToList $ fmap (NodeContent . T.decodeUtf8 . B64.encode) $ saslInitial auth
  proceedAuth $ saslWire auth

  where proceedAuth wire = do
          eresp <- streamRecv s
          let mdat = listToMaybe $ fmap (B64.decode . T.encodeUtf8) $ fromNode (NodeElement eresp) $/ content
          mdat' <- forM mdat $ \case
            Left err -> throwM $ UnexpectedInput $ "proceedAuth, base64 decode: " ++ err
            Right d -> return d
          resp <- case mdat' of
            d | elementName eresp == saslName "success" -> return $ SASLSuccess d
            Nothing | elementName eresp == saslName "failure" -> return $ SASLFailure
            Just dat | elementName eresp == saslName "challenge" -> return $ SASLChallenge dat
            _ -> throwM $ UnexpectedStanza (elementName eresp) $ map saslName ["challenge", "success", "failure"]
          res <- liftIO $ runSASLWire wire resp
          case res of
            Left (msg, wire') -> do
              streamSend s $ case msg of
                SASLResponse r -> element (saslName "response") [] [NodeContent $ T.decodeUtf8 $ B64.encode r]
                SASLAbort -> closedElement (saslName "abort")
              proceedAuth wire'
            Right (Just a) -> do
              $(logInfo) $ "Successfully authenticated"
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
      unless (elementName eanswer == startTLSName "proceed") $ throwM $ UnexpectedStanza (elementName eanswer) [startTLSName "proceed"]
      return FeaturesTLS
  
  | any (\case SASL _ -> True; _ -> False) features = do
      $(logInfo) "Authenticating"
      let (SASL methods):_ = filter (\case SASL _ -> True; _ -> False) features
      r <- saslAuth s $ concatMap (\m -> filter (\a -> saslMechanism a == m) connectionAuth) methods
      return $ if isJust r then FeaturesRestart else FeaturesUnauthorized

  | otherwise = return FeaturesOK

data ClientError = Unauthorized
                 | NoTLSParams
                 deriving (Show, Eq)

createStream :: MonadStream m => ConnectionSettings -> m (Either ClientError (Stream m))
createStream csettings@(ConnectionSettings {..}) =
  bracketOnError (handleConnErr $ connectTo connectionContext connectionParams { connectionUseSecure = Nothing }) (liftIO . connectionClose) initStream

  where
    initStream streamConn = do
      $(logInfo) "Initializing stream"

      streamRender' <- createStreamRender csettings streamConn
      (streamInfo, streamSource') <- createStreamSource streamConn
      streamRender <- newIORef streamRender'
      streamSource <- newIORef streamSource'

      let stream = Stream { streamFeatures = []
                          , ..
                          }
          handleErr e = do
            streamSend stream $ streamErrorMsg e
            streamClose stream
            throwM e
      
      (features, r) <- handle handleErr $ do
        efeatures <- streamRecv stream
        if ssVersion streamInfo /= xmppVersion
          then throwM $ UnsupportedVersion $ ssVersion streamInfo
          else do
            features <- parseFeatures efeatures
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
