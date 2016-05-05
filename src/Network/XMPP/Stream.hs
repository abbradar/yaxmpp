module Network.XMPP.Stream
       ( reinsert
       , sinkVoid
         
       , StreamException(..)
       , StreamSettings(..)
       , streamSettings
       , SStreamSettings(..)
       , StreamFeature(..)
       , Stream(..)
       , ConnectionSettings(..)
       , connectionSettings
       , expectYield
       , runClient
       ) where

import Data.Monoid
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Catch
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
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default
import Text.XML
import Data.XML.Types (Event(..), Content(..))
import qualified Text.XML.Unresolved as XMLU
import qualified Text.XML.Stream.Parse as XMLP
import qualified Text.XML.Stream.Render as XMLR
import Text.XML.Cursor
import Data.IORef

import Network.XMPP.Names

yieldTries :: Monad m => Consumer i m (Maybe a) -> Conduit i m a
yieldTries comp = do
  mr <- comp
  case mr of
    Nothing -> return ()
    Just r -> do
      yield r
      yieldTries comp

reinsert :: Monad m => Consumer a (ConduitM i' a m) ()
reinsert = awaitForever $ lift . yield

-- Fails when gets any input
sinkVoid :: Monad m => Consumer a m ()
sinkVoid = awaitForever $ const $ fail "sinkVoid: input received"

data StreamSettings = StreamSettings { ssVersion :: Text
                                     , ssFrom :: Text
                                     , ssTo :: Text
                                     }
                    deriving (Show)

streamSettings :: Text -> Text -> StreamSettings
streamSettings from to = StreamSettings { ssVersion = "1.0"
                                        , ssFrom = from
                                        , ssTo = to
                                        }

startEvents :: StreamSettings -> [Event]
startEvents (StreamSettings {..}) =
  [ EventBeginDocument
  , EventBeginElement (streamName "stream")
    [ (xmlName "lang", [ContentText "en"])

    , ("version", [ContentText ssVersion])
    , ("from", [ContentText ssFrom])
    , ("to", [ContentText ssTo])
    ]
  ]

stopEvents :: [Event]
stopEvents =
  [ EventEndElement (streamName "stream")
  , EventEndDocument
  ]

data SStreamSettings = SStreamSettings { sssFrom :: Text
                                       , sssId :: Text
                                       , sssVersion :: Text
                                       , sssLang :: Text
                                       }
                     deriving (Show)

streamTag :: MonadThrow m => (SStreamSettings -> ConduitM Event o m c) -> ConduitM Event o m c
streamTag = XMLP.force "Stream has not started" . XMLP.tagName (streamName "stream") streamAttrs
  where streamAttrs = do
          sssLang <- XMLP.requireAttr (xmlName "lang")
          sssId <- XMLP.requireAttr "id"
          sssFrom <- XMLP.requireAttr "from"
          sssVersion <- XMLP.requireAttr "version"
          return SStreamSettings {..}

data StreamFeature = Compression [Text]
                   | StartTLS Bool
                   deriving (Show, Eq)

compressionName :: Text -> Name
compressionName = nsName "http://jabber.org/features/compress"

startTLSName :: Text -> Name
startTLSName = nsName "urn:ietf:params:xml:ns:xmpp-tls"

data StreamException = UnexpectedStanza Name Name
                     | XMLError XMLP.XmlException
                     | UnresolvedEntity (Set Text)
                     deriving (Show, Typeable)

instance Exception StreamException

data ConnectionInterruptionException = ConnectionInterruptionException
                                     | TLSException TLSException
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

parseFeatures :: (MonadThrow m, MonadLogger m) => Element -> m [StreamFeature]
parseFeatures e
  | elementName e == streamName "features" = catMaybes <$> mapM parseOne (elementNodes e)
  | otherwise = throwM $ UnexpectedStanza (elementName e) (streamName "features")

  where parseOne n@(NodeElement f)
          | elementName f == startTLSName "starttls" =
              return $ Just $ StartTLS $ not $ null $ fromNode n $/ checkName (== "required")
          | elementName f == compressionName "compression" =
              return $ Just $ Compression $ fromNode n $/ checkName (== "method") &/ content
          | otherwise = do
              $(logWarn) $ "Unknown feature: " <> T.pack (show f)
              return Nothing

        parseOne _ = return Nothing

data Stream m = Stream { streamSource :: ResumableSource m Element
                       , streamDest :: TMQueue Element
                       , streamFeatures :: [StreamFeature]
                       , streamInfo :: SStreamSettings
                       }

data ConnectionSettings = ConnectionSettings { connectionEndpoint :: TLSClientConfig
                                             , connectionStream :: StreamSettings
                                             }

connectionSettings :: ByteString -> Int -> Text -> Text -> ConnectionSettings
connectionSettings host port server user = ConnectionSettings { connectionEndpoint = tlsClientConfig port host
                                                              , connectionStream = streamSettings (user <> "@" <> server) server
                                                              }

expectYield :: MonadThrow m => Consumer i m i
expectYield = await >>= \case
  Nothing -> throwM ConnectionInterruptionException
  Just a -> return a

runClient :: forall m. (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadCatch m, MonadMask m, MonadLogger m) => ConnectionSettings -> (Stream m -> m ()) -> m ()
runClient cfg comp = runTLSClientStartTLS (connectionEndpoint cfg) $ \(appDataF, startTLS) -> do
  let tryClient :: AppData -> m ()
      tryClient appData = do
        $(logInfo) "Initializing stream"
        streamcfgR <- liftBase $ newIORef Nothing
        -- Separate thread to send messages
        sendQueue <- liftBase $ newTMQueueIO
        let sendMsg = liftBase . atomically . writeTMQueue sendQueue

        sendThread <- liftBase $ async $ do
          sourceTMQueue sendQueue
            $$ streamSend (connectionStream cfg)
            =$= XMLR.renderBuilderFlush renderSettings
            =$= builderToByteStringFlush
            =$= CL.mapMaybe filterFlush
            =$= appSink appData

        let handleError e = do
              sendMsg $ errorMessage e
              liftBase $ atomically $ closeTMQueue sendQueue
              liftBase $ wait sendThread
              throwM e

        handle handleError $ do
          let source = newResumableSource $
                       appSource appData
                       =$= XMLP.parseBytes parseSettings
                       =$= streamReceive streamcfgR
                       =$= CL.mapM tryFromElement

          (source', efeatures) <- source $$++ expectYield
          features <- parseFeatures efeatures
          if | any (\case StartTLS _ -> True; _ -> False) features -> do
                 sendMsg $ closedElement $ startTLSName "starttls"
                 (_, eanswer) <- source' $$++ expectYield
                 unless (elementName eanswer == startTLSName "proceed") $ throwM $ UnexpectedStanza (elementName eanswer) (startTLSName "proceed")
                 liftBase $ cancel sendThread
                 $(logInfo) "Negotiating TLS"
                 handle (throwM . TLSException) $ startTLS tryClient
             | otherwise -> do
                 Just streamcfg <- liftBase $ readIORef streamcfgR
                 comp Stream { streamSource = source'
                             , streamDest = sendQueue
                             , streamFeatures = features
                             , streamInfo = streamcfg
                             }

      renderSettings = def { rsNamespaces = [ ("stream", streamNS)
                                            , ("xml", xmlNS)
                                            ]
                           }

      parseSettings = def

      streamReceive streamcfgR = do
        handleC (throwM . XMLError) $ streamTag $ \streamcfg -> do
          liftBase $ writeIORef streamcfgR $ Just streamcfg
          CL.map (Nothing, ) =$= yieldTries XMLU.elementFromEvents

      streamSend mystreamcfg = do
        mapM_ (yield . Chunk) $ startEvents mystreamcfg
        yield Flush
        awaitForever $ \i -> do
          mapM_ (yield . Chunk) $ XMLU.elementToEvents $ toXMLElement i
          yield Flush
        mapM_ (yield . Chunk) stopEvents

      tryFromElement e' = case fromXMLElement e' of
        Right e -> return e
        Left unres -> throwM $ UnresolvedEntity unres

      filterFlush :: Flush a -> Maybe a
      filterFlush Flush = Nothing
      filterFlush (Chunk a) = Just a

  tryClient appDataF
