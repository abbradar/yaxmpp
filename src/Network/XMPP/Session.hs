module Network.XMPP.Session
       ( MonadSession
       , ResumptionExceptionData(..)
       , ResumptionException(..)
       , Session
       , sessionResource
       , sessionSend
       , sessionStep
       , SessionSettings(..)
       , createSession
       , closeSession
       , cleanupPending
       ) where

import Data.Word
import Data.Monoid
import Text.Read
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted
import Control.Monad.Logger
import Data.IORef.Lifted
import Data.Sequence (Seq, (|>), ViewL(..))
import qualified Data.Sequence as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Catch
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Network.Connection

import Network.XMPP.XML
import Network.XMPP.Stream
import Data.ID (IDGen)
import qualified Data.ID as ID

type MonadSession m = (MonadStream m, MonadBaseControl IO m)

data ResumptionExceptionData = ClientErrorException ClientError
                             | StreamManagementVanished
                             | ResumptionFailed
                             deriving (Show, Eq)

data ResumptionException = ResumptionException ConnectionInterruptionException ResumptionExceptionData
                         deriving (Show, Typeable)

instance Exception ResumptionException

data ReadSessionData m = RSData { rsRequests :: Map Integer (Session m -> Element -> m ())
                                , rsReqIds :: IDGen
                                , rsRecvN :: Maybe Word32
                                }

data WriteSessionData = WSData { wsPending :: Seq (Word32, Element)
                               , wsPendingN :: Word32
                               }

data ReconnectInfo = ReconnectInfo { reconnectId :: Text
                                   , reconnectSettings :: ConnectionSettings
                                   }

data Session m = Session { sessionResource :: Text
                         , sessionHandler :: Session m -> Element -> m ()
                         , sessionReconnect :: Maybe ReconnectInfo
                         , sessionStream :: IORef (Stream m)
                         , sessionRLock :: MVar (ReadSessionData m)
                         , sessionWLock :: MVar (Maybe WriteSessionData)
                         }

applySentH :: Word32 -> WriteSessionData -> WriteSessionData
applySentH h ws = ws { wsPending = go $ wsPending ws }
  where go s = case S.viewl s of
          EmptyL -> s
          (h', _) :< t -> if h' <=? h then go t else s

        a <=? b = (b - a) < (maxBound `div` 2)

tryRestart :: MonadSession m
           => ReconnectInfo
           -> ReadSessionData m
           -> Maybe WriteSessionData
           -> ConnectionInterruptionException
           -> m (Stream m, WriteSessionData)
tryRestart ri@(ReconnectInfo {..}) rs@(RSData { rsRecvN = Just recvn }) (Just ws) e = do
  let throwE = throwM . ResumptionException e
  $(logWarn) "Trying to restart the connection"
  ns <- createStream reconnectSettings
  case ns of
    Left err -> throwE $ ClientErrorException err
    Right s -> do
      unless (any (\case StreamManagement -> True; _ -> False) $ streamFeatures s) $ throwE StreamManagementVanished
      streamSend s $ element (smName "resume") [("h", T.pack $ show recvn), ("previd", reconnectId)] []
      eanswer <- streamRecv s
      if | elementName eanswer == smName "resumed"
           , Just nsent <- readAttr "h" eanswer
           -> do
             let ws' = applySentH nsent ws
             handle (tryRestart ri rs (Just ws')) $ do
               forM_ (wsPending ws') $ \(_, stanza) -> streamSend s stanza
               return (s, ws')
         | otherwise -> throwE ResumptionFailed

tryRestart _ _ _ e = throwM e

modifyRead :: MonadSession m => Session m -> (ReadSessionData m -> m (ReadSessionData m, a)) -> m a
modifyRead sess comp = modifyMVar (sessionRLock sess) tryRun

  where tryRun rs = handle (tryHandle rs) $ comp rs
        tryHandle rs e = case sessionReconnect sess of
          Nothing -> throwM e
          Just ri -> do
            modifyMVar_ (sessionWLock sess) $ \ws -> do
              (s', ws') <- tryRestart ri rs ws e
              writeIORef (sessionStream sess) s'
              return $ Just ws'
            tryRun rs

modifyWrite :: MonadSession m => Session m -> (Maybe WriteSessionData -> m (Maybe WriteSessionData, a)) -> m a
modifyWrite sess comp = modifyMVar (sessionWLock sess) tryRun

  where tryRun ws = handle (tryHandle ws) $ comp ws
        tryHandle ws e = case sessionReconnect sess of
          Nothing -> throwM e
          Just ri -> do
            ws' <- modifyMVar (sessionRLock sess) $ \rs -> do
              (s', ws') <- tryRestart ri rs ws e
              writeIORef (sessionStream sess) s'
              return (rs, Just ws')
            tryRun ws'

sessionSend :: MonadSession m => Session m -> Element -> m ()
sessionSend sess e = modifyWrite sess $ \ws -> do
  s <- readIORef $ sessionStream sess
  let ws' = case ws of
        Nothing -> ws
        Just rws -> let nextp = wsPendingN rws + 1
                    in Just rws { wsPendingN = nextp
                                , wsPending = wsPending rws |> (nextp, e)
                                }
  streamSend s e
  return (ws', ())

handleIq :: MonadSession m => ReadSessionData m -> Element -> m (ReadSessionData m, (Session m -> Element -> m ()))
handleIq ri e = case readAttr "id" e of
  Nothing -> throwM $ UnexpectedInput "sessionStep: no id for iq"
  Just rid -> case M.lookup rid $ rsRequests ri of
    Nothing -> throwM $ UnexpectedInput "sessionStep: invalid id for iq"
    Just handler -> do
      let ri' = ri { rsReqIds = ID.free rid $ rsReqIds ri
                   , rsRequests = M.delete rid $ rsRequests ri
                   }
      return (ri', handler)

handleQ :: MonadSession m => ReadSessionData m -> Session m -> Element -> m ()
handleQ (RSData { rsRecvN = Just n }) sess _ = sessionSend sess $ element (smName "r") [("h", T.pack $ show n)] []
handleQ _ _ _ = fail "handleQ: impossible"

handleR :: MonadSession m => Session m -> Element -> m ()
handleR sess e = case readAttr "h" e of
  Nothing -> throwM $ UnexpectedInput "handleR: invalid h"
  Just nsent -> modifyWrite sess $ \case
    Nothing -> throwM $ UnexpectedInput "handleR: stream management is not enabled"
    Just wi -> return (Just $ applySentH nsent wi, ())

sessionStep :: MonadSession m => Session m -> m ()
sessionStep sess = do
  (handler, emsg) <- modifyRead sess $ \ri -> do
    s <- readIORef $ sessionStream sess
    emsg <- streamRecv s
    let ri' = ri { rsRecvN = fmap (+1) $ rsRecvN ri }
    (ri'', handler) <-
      if | elementName emsg == jcName "iq"
           , Just typ <- getAttr "type" emsg
           , typ == "result" || typ == "error"
            -> handleIq ri emsg 
         | elementName emsg == smName "q" -> return (ri', handleQ ri')
         | elementName emsg == smName "r" -> return (ri', handleR)
         | otherwise -> return (ri', sessionHandler sess)
    return (ri'', (handler, emsg))
  handler sess emsg

data SessionSettings = SessionSettings { ssConn :: ConnectionSettings
                                       , ssResource :: Text
                                       }

bindResource :: MonadSession m => Text -> Stream m -> m Text
bindResource wantRes s
  | not $ any (\case BindResource -> True; _ -> False) $ streamFeatures s = throwM $ UnexpectedInput "bindResource: no resource bind"
  | otherwise = do
      streamSend s $ element "iq" [("type", "set"), ("id", "res-bind")]
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
          $(logInfo) $ "Binded resource: " <> res
          return res
        _ -> throwM $ UnexpectedInput "bindResource: bind failure"

parseLocation :: ConnectionSettings -> Text -> ConnectionSettings
parseLocation csettings string =
  csettings { connectionParams = (connectionParams csettings) { connectionHostname = T.unpack host
                                                              , connectionPort = fromIntegral (port :: Integer)
                                                              }
            }
  where (host, port) = case T.breakOnEnd ":" string of
          (T.init -> nhost, readMaybe . T.unpack -> Just nport) -> (nhost, nport)
          _ -> (string, 5222)

initSM :: MonadSession m => ConnectionSettings -> Stream m -> m (Maybe (Maybe ReconnectInfo))
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
             streamSend s $ element (smName "enable") [] []
             esmr2 <- streamRecv s
             if elementName esmr2 == smName "enabled"
               then do
                 $(logInfo) "Stream management without resumption enabled"
                 return Nothing
               else throwM $ UnexpectedInput "initSM: stream management is advertised but cannot be enabled"

createSession :: MonadSession m => SessionSettings -> (Session m -> Element -> m ()) -> m (Either ClientError (Session m))
createSession (SessionSettings {..}) sessionHandler = do
  ms <- createStream ssConn
  case ms of
    Left e -> return $ Left e
    Right s -> do
      sessionResource <- bindResource ssResource s
      msm <- initSM ssConn s
      sessionStream <- newIORef s
      sessionRLock <- newMVar RSData { rsRequests = M.empty
                                     , rsReqIds = ID.empty
                                     , rsRecvN = 0 <$ msm
                                     }
      sessionWLock <- newMVar $ WSData { wsPending = S.empty
                                       , wsPendingN = 0
                                       } <$ msm
      return $ Right Session { sessionReconnect = join msm
                             , ..
                             }

closeSession :: MonadSession m => Session m -> m ()
closeSession sess = modifyMVar_ (sessionRLock sess) $ \ri -> modifyMVar (sessionWLock sess) $ \wi -> do
  s <- readIORef $ sessionStream sess
  streamClose s
  -- Effectively prevent reconnection
  return (wi, ri { rsRecvN = Nothing })

cleanupPending :: MonadSession m => Session m -> m ()
cleanupPending sess = sessionSend sess $ closedElement $ smName "r"
