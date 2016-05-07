module Network.XMPP.Stream
       ( xmppVersion
       , StreamException(..)
       , StreamSettings(..)
       , StreamFeature(..)
       , Stream(..)
       , ConnectionSettings(..)
       , ClientError(..)
       , tlsClientConfig
       , expectYield
       , runClient
       ) where

import Data.Monoid
import Data.Maybe
import Data.Typeable
import Data.IORef
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Catch
import Control.Concurrent (myThreadId, throwTo)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.ByteString.Builder
import Data.Conduit.Network
import Data.Conduit.Network.TLS
import Network.TLS (TLSException)
import Data.Conduit.TQueue
import Data.Set (Set)
import qualified Data.Map as M
import qualified Blaze.ByteString.Builder as BB
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
import Text.XML.Cursor
import qualified Data.ByteString.Base64 as B64

import Network.SASL
import Network.XMPP.Names

yieldTries :: Monad m => Consumer i m (Maybe a) -> Conduit i m a
yieldTries comp = do
  mr <- comp
  case mr of
    Nothing -> return ()
    Just r -> do
      yield r
      yieldTries comp

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
    ]
  ]

showElement :: Element -> Text
showElement e = T.decodeUtf8 $ BB.toByteString $ mconcat bs
  where bs = runIdentity $ CL.sourceList (XMLU.elementToEvents $ toXMLElement e) $$ XMLR.renderBuilder def =$= CL.consume
  
stopEvents :: [Event]
stopEvents =
  [ EventEndElement (streamName "stream")
  , EventEndDocument
  ]

data StreamSettings = StreamSettings { ssFrom :: Text
                                     , ssId :: Text
                                     , ssVersion :: Text
                                     , ssLang :: Text
                                     }
                     deriving (Show, Eq)

streamTag :: MonadThrow m => (StreamSettings -> ConduitM Event o m c) -> ConduitM Event o m c
streamTag = XMLP.force "Stream has not started" . XMLP.tagName (streamName "stream") streamAttrs
  where streamAttrs = do
          ssLang <- XMLP.requireAttr (xmlName "lang")
          ssId <- XMLP.requireAttr "id"
          ssFrom <- XMLP.requireAttr "from"
          ssVersion <- XMLP.requireAttr "version"
          return StreamSettings {..}

data StreamFeature = Compression [Text]
                   | StartTLS Bool
                   | SASL [ByteString]
                   deriving (Show, Eq)

compressionName :: Text -> Name
compressionName = nsName "http://jabber.org/features/compress"

startTLSName :: Text -> Name
startTLSName = nsName "urn:ietf:params:xml:ns:xmpp-tls"

saslName :: Text -> Name
saslName = nsName "urn:ietf:params:xml:ns:xmpp-sasl"

data StreamException = UnexpectedStanza Name Name
                     | XMLError XMLP.XmlException
                     | UnresolvedEntity (Set Text)
                     | UnsupportedVersion Text
                     | InvalidBase64 String
                     deriving (Show, Typeable)

instance Exception StreamException

data ConnectionInterruptionException = ConnectionInterruptionException
                                     | TLSException TLSException
                                     | SenderException SomeException
                                     deriving (Show, Typeable)

instance Exception ConnectionInterruptionException

estreamName :: T.Text -> Name
estreamName = nsName "urn:ietf:params:xml:ns:xmpp-streams"

closedElement :: Name -> Element
closedElement name = Element { elementName = name
                             , elementAttributes = M.empty
                             , elementNodes = []
                             }

errorMessage :: StreamException -> Element
errorMessage (UnexpectedStanza _ _) = closedElement $ estreamName "invalid-xml"
errorMessage (XMLError _) = closedElement $ estreamName "not-well-formed"
errorMessage (UnresolvedEntity _) = closedElement $ estreamName "not-well-formed"
errorMessage (UnsupportedVersion _) = closedElement $ estreamName "unsupported-version"
errorMessage (InvalidBase64 _) = closedElement $ estreamName "invalid-xml"

parseFeatures :: (MonadThrow m, MonadLogger m) => Element -> m [StreamFeature]
parseFeatures e
  | elementName e == streamName "features" = catMaybes <$> mapM parseOne (elementNodes e)
  | otherwise = throwM $ UnexpectedStanza (elementName e) (streamName "features")

  where parseOne n@(NodeElement f)
          | elementName f == startTLSName "starttls" =
              return $ Just $ StartTLS $ not $ null $ fromNode n $/ checkName (== startTLSName "required")
          | elementName f == compressionName "compression" =
              return $ Just $ Compression $ fromNode n $/ checkName (== compressionName "method") &/ content
          | elementName f == saslName "mechanisms" =
              return $ Just $ SASL $ map T.encodeUtf8 $ fromNode n $/ checkName (== saslName "mechanism") &/ content
          | otherwise = do
              $(logWarn) $ "Unknown feature: " <> showElement f
              return Nothing

        parseOne _ = return Nothing

data Stream = Stream { streamSource :: ResumableSource IO Element
                     , streamDest :: Element -> IO ()
                     , streamFeatures :: [StreamFeature]
                     , streamInfo :: StreamSettings
                     }

data ConnectionSettings = ConnectionSettings { connectionEndpoint :: TLSClientConfig
                                             , connectionServer :: Text
                                             , connectionUser :: Text
                                             , connectionAuth :: [SASLAuthenticator ()]
                                             }

data StopSenderException = StopSenderException
                         deriving (Show, Typeable)

instance Exception StopSenderException

data UnauthorizedException = UnauthorizedException
                         deriving (Show, Typeable)

instance Exception UnauthorizedException

cancelSender :: MonadIO m => Async a -> m ()
cancelSender a = liftIO $ throwTo (asyncThreadId a) StopSenderException

maybeChunk :: Flush a -> Maybe a
maybeChunk Flush = Nothing
maybeChunk (Chunk a) = Just a

expectYield :: (MonadIO m, MonadThrow m) => ResumableSource IO Element -> m (ResumableSource IO Element, Element)
expectYield src = do
  (src', mr) <- liftIO $ src $$++ CL.head
  case mr of
    Nothing -> throwM ConnectionInterruptionException
    Just r -> return (src', r)

streamReceive :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => IORef (Maybe StreamSettings) -> Conduit Event m Element
streamReceive streamcfgR = handleC (throwM . XMLError) $ streamTag $ \streamcfg -> do
  liftBase $ writeIORef streamcfgR $ Just streamcfg
  CL.map (Nothing, ) =$= yieldTries XMLU.elementFromEvents =$= CL.mapM tryFromElement

  where tryFromElement e' = case fromXMLElement e' of
          Right e -> do
            runStderrLoggingT $ $(logDebug) $ "Received message: " <> showElement e
            return e
          Left unres -> throwM $ UnresolvedEntity unres

streamSend :: (MonadThrow m, MonadIO m) => Text -> Text -> Conduit Element m (Flush Event)
streamSend from to = do
  mapM_ (yield . Chunk) $ startEvents from to
  yield Flush
  awaitForever $ \e -> do
    runStderrLoggingT $ $(logDebug) $ "Sending message: " <> showElement e
    mapM_ (yield . Chunk) $ XMLU.elementToEvents $ toXMLElement e
    yield Flush
  mapM_ (yield . Chunk) stopEvents

data ClientError = Unauthorized
                 deriving (Show, Eq)

runClient :: forall m a. (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadCatch m, MonadMask m, MonadLogger m) => ConnectionSettings -> (Stream -> m a) -> m (Either ClientError a)
runClient (ConnectionSettings {..}) comp = runTLSClientStartTLS connectionEndpoint $ \(appDataF, startTLS) -> do
  let tryClient :: AppData -> m (Either ClientError a)
      tryClient appData = do
        $(logInfo) "Initializing stream"
        -- Separate thread to send messages
        sendQueue <- liftIO newTMQueueIO
        myid <- liftIO myThreadId
        let sendLoop =
              handle (throwTo myid . SenderException) $
              handle (\StopSenderException -> return ()) $
                sourceTMQueue sendQueue
                $$ streamSend (connectionUser <> "@" <> connectionServer) connectionServer
                =$= XMLR.renderBuilderFlush renderSettings
                =$= builderToByteStringFlush
                =$= CL.mapMaybe maybeChunk
                =$= appSink appData

        bracket (liftIO $ async sendLoop) cancelSender $ \sendThread -> do
          streamcfgR <- liftIO $ newIORef Nothing
          let source = newResumableSource $
                       appSource appData
                       =$= XMLP.parseBytes parseSettings
                       =$= streamReceive streamcfgR
          
          let handleError e = liftIO $ do
                atomically $ do
                  writeTMQueue sendQueue $ errorMessage e
                  closeTMQueue sendQueue
                wait sendThread
                throwM e

          handle handleError $ do
            let sendMsg = liftIO . atomically . writeTMQueue sendQueue
            (source', efeatures) <- expectYield source
            Just streamcfg <- liftIO $ readIORef streamcfgR
            unless (ssVersion streamcfg == xmppVersion) $ throwM $ UnsupportedVersion $ ssVersion streamcfg
            features <- parseFeatures efeatures

            if | any (\case StartTLS _ -> True; _ -> False) features -> do
                   $(logInfo) "Negotiating TLS"
                   sendMsg $ closedElement $ startTLSName "starttls"
                   (_, eanswer) <- expectYield source'
                   unless (elementName eanswer == startTLSName "proceed") $ throwM $ UnexpectedStanza (elementName eanswer) (startTLSName "proceed")
                   cancelSender sendThread
                   handle (throwM . TLSException) $ do
                     rr <- liftIO $ newIORef Nothing
                     startTLS $ \appData' -> do
                       r <- tryClient appData'
                       liftIO $ writeIORef rr $ Just r
                     Just r <- liftIO $ readIORef rr
                     return r
  
               | any (\case SASL _ -> True; _ -> False) features -> do
                   $(logInfo) "Authenticating"
                   let (SASL methods):_ = filter (\case SASL _ -> True; _ -> False) features
                   r <- saslAuth source' sendMsg $ concatMap (\m -> filter (\a -> saslMechanism a == m) connectionAuth) methods
                   if isJust r
                     then do
                       cancelSender sendThread
                       tryClient appData
                     else return $ Left Unauthorized

               | otherwise -> do
                   r <- comp Stream { streamSource = source'
                                    , streamDest = atomically . writeTMQueue sendQueue
                                    , streamFeatures = features
                                    , streamInfo = streamcfg
                                    }
                   liftIO $ atomically $ closeTMQueue sendQueue
                   liftIO $ wait sendThread
                   return $ Right r

      saslAuth :: ResumableSource IO Element -> (Element -> m ()) -> [SASLAuthenticator r] -> m (Maybe r)
      saslAuth _ _ [] = return Nothing
      saslAuth source0 sendMsg (auth:others) = do
        $(logInfo) $ "Trying auth method " <> T.decodeUtf8 (saslMechanism auth)
        sendMsg Element { elementName = saslName "auth"
                        , elementAttributes = M.fromList [("mechanism", T.decodeUtf8 $ saslMechanism auth)]
                        , elementNodes = maybeToList $ fmap (NodeContent . T.decodeUtf8 . B64.encode) $ saslInitial auth
                        }
        proceedAuth source0 $ saslWire auth

        where proceedAuth source wire = do
                (source', eresp) <- expectYield source
                let mdat = listToMaybe $ fmap (B64.decode . T.encodeUtf8) $ fromNode (NodeElement eresp) $/ content
                mdat' <- forM mdat $ \case
                  Left err -> throwM $ InvalidBase64 err
                  Right d -> return d
                resp <- case mdat' of
                  d | elementName eresp == saslName "success" -> return $ SASLSuccess d
                  Nothing | elementName eresp == saslName "failure" -> return $ SASLFailure
                  Just dat | elementName eresp == saslName "challenge" -> return $ SASLChallenge dat
                  _ -> throwM $ UnexpectedStanza (elementName eresp) "challenge | success | failure"
                res <- liftBase $ runSASLWire wire resp
                case res of
                  Left (msg, wire') -> do
                    sendMsg $ case msg of
                      SASLResponse r -> Element { elementName = saslName "response"
                                                   , elementAttributes = M.empty
                                                   , elementNodes = [NodeContent $ T.decodeUtf8 $ B64.encode r]
                                                   }
                      SASLAbort -> closedElement $ saslName "abort"
                    proceedAuth source' wire'
                  Right (Just a) -> return $ Just a
                  Right Nothing -> saslAuth source' sendMsg others

      renderSettings = def { rsNamespaces = [ ("stream", streamNS)
                                            , ("xml", xmlNS)
                                            ]
                           }
  
      parseSettings = def

  tryClient appDataF
