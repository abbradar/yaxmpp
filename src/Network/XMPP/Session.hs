{-# LANGUAGE Strict #-}

module Network.XMPP.Session (
  ResumptionExceptionData (..),
  ResumptionException (..),
  Session,
  sessionAddress,
  sessionStreamFeatures,
  sessionSend,
  SessionStep (..),
  sessionStep,
  SessionSettings (..),
  sessionCreate,
  sessionClose,
  sessionKill,
  sessionIsClosed,
  sessionPeriodic,
  sessionGetStream,
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.Word
import Text.Read (readMaybe)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)
import UnliftIO.IORef
import UnliftIO.MVar

import Network.Connection
import Network.XMPP.Address
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML

data ResumptionExceptionData
  = ClientErrorException ClientError
  | StreamManagementVanished
  | ResumptionFailed
  deriving (Show, Eq)

data ResumptionException = ResumptionException ConnectionInterruptionException ResumptionExceptionData
  deriving (Show, Typeable)

instance Exception ResumptionException

-- | Stream management read-side data.
data SMReadData = SMReadData
  { smRecvN :: Word32
  }

-- | Stream management write-side data.
data SMWriteData = SMWriteData
  { smPending :: Seq (Word32, Element)
  , smPendingN :: Word32
  }

data ReadSessionData = ReadSessionData
  { rsStreamManagement :: Maybe SMReadData
  , rsReconnected :: Bool
  }

data WriteSessionData = WriteSessionData
  { wsStreamManagement :: Maybe SMWriteData
  }

data ReconnectInfo = ReconnectInfo
  { reconnectId :: Text
  , reconnectSettings :: ConnectionSettings
  }

data Session m = Session
  { sessionAddress :: FullJID
  , sessionStreamFeatures :: [Element]
  , sessionReconnect :: Maybe ReconnectInfo
  , sessionStream :: IORef (Stream m)
  , sessionStreamGen :: IORef Int
  , sessionRLock :: MVar ReadSessionData
  , sessionWLock :: MVar WriteSessionData
  }

bindName :: Text -> Name
bindName = nsName "urn:ietf:params:xml:ns:xmpp-bind"

smNS :: Text
smName :: Text -> Name
(smNS, smName) = namePair "urn:xmpp:sm:3"

isSM :: Element -> Bool
isSM e = elementName e == smName "sm"

applySentH :: Word32 -> SMWriteData -> SMWriteData
applySentH h ws = ws {smPending = go $ smPending ws}
 where
  go s = case S.viewl s of
    EmptyL -> s
    (h', _) :< t -> if h' <=? h then go t else s

  a <=? b = (b - a) < (maxBound `div` 2)

restartOrThrow ::
  (MonadStream m) =>
  ReconnectInfo ->
  ReadSessionData ->
  WriteSessionData ->
  ConnectionInterruptionException ->
  m (Stream m, WriteSessionData)
restartOrThrow ri@(ReconnectInfo {..}) rs ws e =
  case (rsStreamManagement rs, wsStreamManagement ws) of
    (Just smr, Just smw) -> do
      let throwE = throwM . ResumptionException e
      $(logWarn) "Trying to restart the connection"
      ns <- streamCreate reconnectSettings
      case ns of
        Left err -> throwE $ ClientErrorException err
        Right s -> do
          unless (any isSM $ streamFeatures s) $ void $ throwE StreamManagementVanished
          streamSend s $ element (smName "resume") [("h", showt $ smRecvN smr), ("previd", reconnectId)] []
          eanswer <- streamRecv s
          if
            | elementName eanswer == smName "resumed"
            , Just nsent <- readAttr "h" eanswer ->
                do
                  let smw' = applySentH nsent smw
                      ws' = ws {wsStreamManagement = Just smw'}
                  handle (restartOrThrow ri rs ws') $ do
                    forM_ (smPending smw') $ \(_, stanza) -> streamSend s stanza
                    return (s, ws')
            | otherwise -> throwE ResumptionFailed
    _ -> throwM e

tryRestart :: (MonadStream m) => Session m -> ReconnectInfo -> ReadSessionData -> WriteSessionData -> ConnectionInterruptionException -> m WriteSessionData
tryRestart (Session {..}) ri rs ws e = do
  s <- readIORef sessionStream
  streamKill s
  closed <- streamIsClosed s
  if closed
    then throwM e
    else do
      (s', ws') <- restartOrThrow ri rs ws e
      writeIORef sessionStream s'
      atomicModifyIORef' sessionStreamGen $ \gen -> (gen + 1, ())
      return ws'

modifyRead :: forall m a. (MonadStream m) => Session m -> (ReadSessionData -> m (ReadSessionData, a)) -> m a
modifyRead sess@(Session {..}) comp = modifyMVar sessionRLock tryRun
 where
  tryRun rs = handle (tryHandle rs) $ comp rs
  tryHandle :: ReadSessionData -> ConnectionInterruptionException -> m (ReadSessionData, a)
  tryHandle rs e = case sessionReconnect of
    Nothing -> throwM e
    Just ri -> do
      modifyMVar_ sessionWLock $ \ws -> do
        ws' <- tryRestart sess ri rs ws e
        return ws'
      tryRun $ rs {rsReconnected = True}

modifyWrite :: forall m a. (MonadStream m) => Session m -> (WriteSessionData -> m (WriteSessionData, a)) -> m a
modifyWrite sess@(Session {..}) comp = do
  result <- modifyMVar sessionWLock tryRun
  handleResult result
 where
  tryRun ws = handle (saveException ws) $ do
    (ws', a) <- comp ws
    return (ws', Right a)

  saveException :: WriteSessionData -> ConnectionInterruptionException -> m (WriteSessionData, Either (ConnectionInterruptionException, Int) a)
  saveException ws e = do
    -- Remember the stream generation before releasing the write lock.
    gen <- readIORef sessionStreamGen
    return (ws, Left (e, gen))

  handleResult (Right a) = return a
  handleResult (Left (e, gen)) =
    case sessionReconnect of
      Nothing -> throwM e
      Just ri -> do
        -- We want to release the read lock later if successful.
        result <- mask $ \restore -> do
          rs <- takeMVar sessionRLock

          let continueReconnect ws = do
                -- Check if another thread already reconnected while we were waiting.
                currentGen <- readIORef sessionStreamGen
                if currentGen /= gen
                  then return ws
                  else tryRestart sess ri rs ws e

          modifyMVar sessionWLock $ \ws -> do
            ws' <- restore (continueReconnect ws) `onException` putMVar sessionRLock rs
            putMVar sessionRLock $ rs {rsReconnected = True}
            restore $ tryRun ws'
        handleResult result

sessionSend :: (MonadStream m) => Session m -> Element -> m ()
sessionSend sess e = modifyWrite sess $ \ws -> do
  s <- readIORef $ sessionStream sess
  let ws' = case wsStreamManagement ws of
        Just smw
          | nameNamespace (elementName e) /= Just smNS ->
              let nextp = smPendingN smw + 1
               in ws
                    { wsStreamManagement =
                        Just
                          smw
                            { smPendingN = nextp
                            , smPending = smPending smw |> (nextp, e)
                            }
                    }
        _ -> ws
  streamSend s e
  return (ws', ())

sessionThrow :: (MonadStream m) => Session m -> StreamError -> m a
sessionThrow sess e = do
  s <- readIORef $ sessionStream sess
  streamThrow s e

handleR :: (MonadStream m) => ReadSessionData -> Session m -> m ()
handleR rs sess = case rsStreamManagement rs of
  Just smr -> do
    s <- readIORef $ sessionStream sess
    closed <- streamIsClosed s
    unless closed $ sessionSend sess $ element (smName "a") [("h", showt $ smRecvN smr)] []
  Nothing -> fail "handleR: impossible"

handleA :: (MonadStream m) => Session m -> Element -> m ()
handleA sess e = modifyWrite sess $ \ws -> case readAttr "h" e of
  Nothing -> sessionThrow sess $ unexpectedInput "handleA: invalid h"
  Just nsent -> case wsStreamManagement ws of
    Nothing -> sessionThrow sess $ unexpectedInput "handleA: stream management is not enabled"
    Just smw -> return (ws {wsStreamManagement = Just $ applySentH nsent smw}, ())

-- | Result of a session step.
data SessionStep
  = -- | A stanza was received.
    SessionStanza Element
  | -- | The session was (re)connected since the last step.
    SessionReconnected

sessionStep :: (MonadStream m) => Session m -> m (Maybe SessionStep)
sessionStep sess = do
  (handler, res) <- modifyRead sess $ \rs -> do
    -- Check if we reconnected since last step
    if rsReconnected rs
      then return (rs {rsReconnected = False}, (return (), Just SessionReconnected))
      else do
        s <- readIORef $ sessionStream sess
        emsg <- streamRecv s
        if
          | elementName emsg == smName "r" -> return (rs, (handleR rs sess, Nothing))
          | elementName emsg == smName "a" -> return (rs, (handleA sess emsg, Nothing))
          | otherwise -> do
              let rs' = rs {rsStreamManagement = fmap (\smr -> smr {smRecvN = smRecvN smr + 1}) $ rsStreamManagement rs}
              return (rs', (return (), Just $ SessionStanza emsg))
  handler
  return res

parseLocation :: ConnectionSettings -> Text -> ConnectionSettings
parseLocation csettings string =
  csettings
    { connectionParams =
        (connectionParams csettings)
          { connectionHostname = T.unpack host
          , connectionPort = fromIntegral (port :: Integer)
          }
    }
 where
  (host, port) = case T.breakOnEnd ":" string of
    (T.init -> nhost, readMaybe . T.unpack -> Just nport) -> (nhost, nport)
    _ -> (string, 5222)

initSM :: (MonadStream m) => ConnectionSettings -> Stream m -> m (Maybe (Maybe ReconnectInfo))
initSM csettings s
  | not $ any isSM $ streamFeatures s = do
      $(logInfo) "Stream management is not supported"
      return Nothing
  | otherwise =
      Just <$> do
        streamSend s $ element (smName "enable") [("resume", "true")] []
        esmr <- streamRecv s
        if
          | elementName esmr == smName "enabled"
          , Just reconnectId <- getAttr "id" esmr
          , mloc <- getAttr "location" esmr ->
              do
                let reconnectSettings = maybe csettings (parseLocation csettings) mloc
                $(logInfo) "Stream management with resumption enabled"
                return $ Just ReconnectInfo {..}
          | otherwise -> do
              -- FIXME: maybe it's better to just bail out -- resumption-less SM eats traffic without much benefit
              streamSend s $ element (smName "enable") [] []
              esmr2 <- streamRecv s
              if elementName esmr2 == smName "enabled"
                then $(logInfo) "Stream management without resumption enabled"
                else $(logWarn) "Stream management is advertised but cannot be enabled"
              return Nothing

data SessionSettings = SessionSettings
  { ssConn :: ConnectionSettings
  , ssResource :: Text
  }

isBind :: Element -> Bool
isBind e = elementName e == bindName "bind"

bindResource :: (MonadStream m) => Text -> Stream m -> m Text
bindResource wantRes s
  | not $ any isBind $ streamFeatures s = streamThrow s $ unexpectedInput "bindResource: no resource bind"
  | otherwise = do
      streamSend s $
        element
          (jcName "iq")
          [("type", "set"), ("id", "res-bind")]
          [ NodeElement $
              element
                (bindName "bind")
                []
                [ NodeElement $
                    element (bindName "resource") [] [NodeContent wantRes]
                ]
          ]
      ebind <- streamRecv s
      case fromElement ebind
        $| XC.element (jcName "iq")
        >=> attributeIs "id" "res-bind"
        >=> attributeIs "type" "result"
        &/ XC.element (bindName "bind")
        &/ XC.element (bindName "jid")
        &/ content of
        res : _ -> do
          $(logInfo) [i|Bound resource: #{res}|]
          return res
        _ -> streamThrow s $ unexpectedInput "bindResource: bind failure"

sessionCreate :: (MonadStream m) => SessionSettings -> m (Either ClientError (Session m))
sessionCreate (SessionSettings {..}) = do
  ms <- streamCreate ssConn
  case ms of
    Left e -> return $ Left e
    Right s -> flip onException (streamClose s) $ do
      address <- bindResource ssResource s
      sessionAddress <- case toRight (xmppAddress address) >>= fullJidGet of
        Just r -> return r
        _ -> fail "sessionCreate: can't normalize address"
      let sessionStreamFeatures = streamFeatures s
      msm <- initSM ssConn s
      sessionStream <- newIORef s
      sessionStreamGen <- newIORef 0
      let smRead = SMReadData {smRecvN = 0} <$ msm
          smWrite = SMWriteData {smPending = S.empty, smPendingN = 0} <$ msm
      sessionRLock <-
        newMVar
          ReadSessionData
            { rsStreamManagement = smRead
            , rsReconnected = True -- Fire reconnect hooks on first step
            }
      sessionWLock <-
        newMVar
          WriteSessionData
            { wsStreamManagement = smWrite
            }
      let sess =
            Session
              { sessionReconnect = join msm
              , ..
              }
      return $ Right sess

sessionClose :: (MonadStream m) => Session m -> m ()
sessionClose sess = modifyMVar_ (sessionWLock sess) $ \ws -> do
  s <- readIORef $ sessionStream sess
  streamClose s
  return ws

sessionKill :: (MonadStream m) => Session m -> m ()
sessionKill sess = modifyMVar_ (sessionRLock sess) $ \rs -> modifyMVar (sessionWLock sess) $ \ws -> do
  s <- readIORef $ sessionStream sess
  streamKill s
  return (ws, rs)

sessionIsClosed :: (MonadStream m) => Session m -> m Bool
sessionIsClosed sess = modifyMVar (sessionWLock sess) $ \ws -> do
  s <- readIORef $ sessionStream sess
  closed <- streamIsClosed s
  return (ws, closed)

sessionPeriodic :: (MonadStream m) => Session m -> m ()
sessionPeriodic sess@(Session {sessionReconnect = Just _}) = sessionSend sess $ closedElement $ smName "r"
sessionPeriodic _ = return ()

sessionGetStream :: (MonadStream m) => Session m -> m (Stream m)
sessionGetStream (Session {..}) = readIORef sessionStream
