module Network.XMPP.Session
       ( ResumptionExceptionData(..)
       , ResumptionException(..)
       , Session
       , sessionAddress
       , sessionSend
       , sessionStep
       , SessionSettings(..)
       , sessionCreate
       , sessionClose
       , sessionPeriodic
       , sessionGetStream
       ) where

import Data.Word
import Text.Read
import Data.Typeable
import Control.Monad
import Control.Concurrent.MVar.Lifted
import Control.Monad.Logger
import Data.IORef.Lifted
import Data.Sequence (Seq, (|>), ViewL(..))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Catch
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Network.Connection
import Text.InterpolatedString.Perl6 (qq)
import TextShow

import Network.XMPP.XML
import Network.XMPP.Stream
import Network.XMPP.Address
import Network.XMPP.Utils

data ResumptionExceptionData = ClientErrorException ClientError
                             | StreamManagementVanished
                             | ResumptionFailed
                             deriving (Show, Eq)

data ResumptionException = ResumptionException ConnectionInterruptionException ResumptionExceptionData
                         deriving (Show, Typeable)

instance Exception ResumptionException

data ReadSessionData = RSData { rsRecvN :: Word32
                              }

data WriteSessionData = WSData { wsPending :: Seq (Word32, Element)
                               , wsPendingN :: Word32
                               }

data ReconnectInfo = ReconnectInfo { reconnectId :: Text
                                   , reconnectSettings :: ConnectionSettings
                                   }

data Session m = Session { sessionAddress :: FullJID
                         , sessionReconnect :: Maybe ReconnectInfo
                         , sessionStream :: IORef (Stream m)
                         , sessionRLock :: MVar (Maybe ReadSessionData)
                         , sessionWLock :: MVar (Maybe WriteSessionData)
                         }

applySentH :: Word32 -> WriteSessionData -> WriteSessionData
applySentH h ws = ws { wsPending = go $ wsPending ws }
  where go s = case S.viewl s of
          EmptyL -> s
          (h', _) :< t -> if h' <=? h then go t else s

        a <=? b = (b - a) < (maxBound `div` 2)

tryRestart :: MonadStream m
           => ReconnectInfo
           -> Maybe ReadSessionData
           -> Maybe WriteSessionData
           -> ConnectionInterruptionException
           -> m (Stream m, WriteSessionData)
tryRestart ri@(ReconnectInfo {..}) (Just rs) (Just ws) e = do
  let throwE = throwM . ResumptionException e
  $(logWarn) "Trying to restart the connection"
  ns <- streamCreate reconnectSettings
  case ns of
    Left err -> throwE $ ClientErrorException err
    Right s -> do
      unless (any (\case StreamManagement -> True; _ -> False) $ streamFeatures s) $ throwE StreamManagementVanished
      streamSend s $ element (smName "resume") [("h", showt $ rsRecvN rs), ("previd", reconnectId)] []
      eanswer <- streamRecv s
      if | elementName eanswer == smName "resumed"
           , Just nsent <- readAttr "h" eanswer
           -> do
             let ws' = applySentH nsent ws
             handle (tryRestart ri (Just rs) (Just ws')) $ do
               forM_ (wsPending ws') $ \(_, stanza) -> streamSend s stanza
               return (s, ws')
         | otherwise -> throwE ResumptionFailed

tryRestart _ _ _ e = throwM e

modifyRead :: MonadStream m => Session m -> (Maybe ReadSessionData -> m (Maybe ReadSessionData, a)) -> m a
modifyRead (Session {..}) comp = modifyMVar sessionRLock tryRun

  where tryRun rs = handle (tryHandle rs) $ comp rs
        tryHandle rs e = case sessionReconnect of
          Nothing -> throwM e
          Just ri -> do
            modifyMVar_ sessionWLock $ \ws -> do
              (s', ws') <- tryRestart ri rs ws e
              writeIORef sessionStream s'
              return $ Just ws'
            tryRun rs

modifyWrite :: MonadStream m => Session m -> (Maybe WriteSessionData -> m (Maybe WriteSessionData, a)) -> m a
modifyWrite (Session {..}) comp = modifyMVar sessionWLock tryRun

  where tryRun ws = handle (tryHandle ws) $ comp ws
        tryHandle ws e = case sessionReconnect of
          Nothing -> throwM e
          Just ri -> do
            ws' <- modifyMVar sessionRLock $ \rs -> do
              (s', ws') <- tryRestart ri rs ws e
              writeIORef sessionStream s'
              return (rs, Just ws')
            tryRun ws'

sessionSend :: MonadStream m => Session m -> Element -> m ()
sessionSend sess e = modifyWrite sess $ \ws -> do
  s <- readIORef $ sessionStream sess
  let ws' = case ws of
        Just rws | nameNamespace (elementName e) /= Just smNS ->
                   let nextp = wsPendingN rws + 1
                   in Just rws { wsPendingN = nextp
                               , wsPending = wsPending rws |> (nextp, e)
                               }
        _ -> ws
  streamSend s e
  return (ws', ())

sessionThrow :: MonadStream m => Session m -> StreamError -> m a
sessionThrow sess e = do
  s <- readIORef $ sessionStream sess
  streamThrow s e

handleR :: MonadStream m => Maybe ReadSessionData -> Session m -> m ()
handleR (Just rs) sess = sessionSend sess $ element (smName "a") [("h", showt $ rsRecvN rs)] []
handleR _ _ = fail "handleQ: impossible"

handleA :: MonadStream m => Session m -> Element -> m ()
handleA sess e = modifyWrite sess $ \ws -> case readAttr "h" e of
  Nothing -> sessionThrow sess $ unexpectedInput "handleR: invalid h"
  Just nsent -> case ws of
    Nothing -> sessionThrow sess $ unexpectedInput "handleR: stream management is not enabled"
    Just wi -> return (Just $ applySentH nsent wi, ())

sessionStep :: MonadStream m => Session m -> m (Maybe Element)
sessionStep sess = do
  (handler, res) <- modifyRead sess $ \ri -> do
    s <- readIORef $ sessionStream sess
    emsg <- streamRecv s
    let riInc = fmap (\x -> x { rsRecvN = rsRecvN x + 1 }) ri
    if | elementName emsg == smName "r" -> return (ri, (handleR ri sess, Nothing))
       | elementName emsg == smName "a" -> return (ri, (handleA sess emsg, Nothing))
       | otherwise -> return (riInc, (return (), Just emsg))
  handler
  return res

data SessionSettings = SessionSettings { ssConn :: ConnectionSettings
                                       , ssResource :: Text
                                       }

bindResource :: MonadStream m => Text -> Stream m -> m Text
bindResource wantRes s
  | not $ any (\case BindResource -> True; _ -> False) $ streamFeatures s = streamThrow s $ unexpectedInput "bindResource: no resource bind"
  | otherwise = do
      streamSend s $ element (jcName "iq") [("type", "set"), ("id", "res-bind")]
        [ NodeElement $ element (bindName "bind") []
          [ NodeElement $ element (bindName "resource") []
            [ NodeContent wantRes
            ]
          ]
        ]
      ebind <- streamRecv s
      case fromElement ebind
           $| XC.element (jcName "iq")
           >=> attributeIs "id" "res-bind"
           >=> attributeIs "type" "result"
           &/ XC.element (bindName "bind")
           &/ XC.element (bindName "jid")
           &/ content
        of
        res:_ -> do
          $(logInfo) [qq|Bound resource: $res|]
          return res
        _ -> streamThrow s $ unexpectedInput "bindResource: bind failure"

parseLocation :: ConnectionSettings -> Text -> ConnectionSettings
parseLocation csettings string =
  csettings { connectionParams = (connectionParams csettings) { connectionHostname = T.unpack host
                                                              , connectionPort = fromIntegral (port :: Integer)
                                                              }
            }
  where (host, port) = case T.breakOnEnd ":" string of
          (T.init -> nhost, readMaybe . T.unpack -> Just nport) -> (nhost, nport)
          _ -> (string, 5222)

initSM :: MonadStream m => ConnectionSettings -> Stream m -> m (Maybe (Maybe ReconnectInfo))
initSM csettings s
  | not $ any (\case StreamManagement -> True; _ -> False) $ streamFeatures s = do
      $(logInfo) "Stream management is not supported"
      return Nothing
  | otherwise = Just <$> do
      streamSend s $ element (smName "enable") [("resume", "true")] []
      esmr <- streamRecv s
      if | elementName esmr == smName "enabled"
           , Just reconnectId <- getAttr "id" esmr
           , mloc <- getAttr "location" esmr
           -> do
               let reconnectSettings = maybe csettings (parseLocation csettings) mloc
               $(logInfo) "Stream management with resumption enabled"
               return $ Just ReconnectInfo { .. }
         | otherwise -> do
             -- FIXME: maybe it's better to just bail out -- resumption-less SM eats traffic without much benefit
             streamSend s $ element (smName "enable") [] []
             esmr2 <- streamRecv s
             if elementName esmr2 == smName "enabled"
               then $(logInfo) "Stream management without resumption enabled"
               else $(logWarn) "Stream management is advertised but cannot be enabled"
             return Nothing

sessionCreate :: MonadStream m => SessionSettings -> m (Either ClientError (Session m))
sessionCreate (SessionSettings {..}) = do
  ms <- streamCreate ssConn
  case ms of
    Left e -> return $ Left e
    Right s -> flip onException (streamClose s) $ do
      address <- bindResource ssResource s
      sessionAddress <- case parseValue xmppAddress address >>= fullJidGet of
        Just r -> return r
        _ -> fail "sessionCreate: can't normalize address"
      msm <- initSM ssConn s
      sessionStream <- newIORef s
      sessionRLock <- newMVar $ RSData { rsRecvN = 0
                                      } <$ msm
      sessionWLock <- newMVar $ WSData { wsPending = S.empty
                                       , wsPendingN = 0
                                       } <$ msm
      let sess = Session { sessionReconnect = join msm
                         , ..
                         }
      return $ Right sess
      
sessionClose :: MonadStream m => Session m -> m ()
sessionClose sess = modifyMVar_ (sessionRLock sess) $ \_ -> modifyMVar (sessionWLock sess) $ \_ -> do
  s <- readIORef $ sessionStream sess
  streamClose s
  -- Effectively prevent reconnection
  return (Nothing, Nothing)

sessionPeriodic :: MonadStream m => Session m -> m ()
sessionPeriodic sess@(Session { sessionReconnect = Just _ }) = sessionSend sess $ closedElement $ smName "r"
sessionPeriodic _ = return ()

sessionGetStream :: MonadStream m => Session m -> m (Stream m)
sessionGetStream (Session {..}) = readIORef sessionStream
