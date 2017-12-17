import Control.Monad
import System.Environment
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import Control.Exception (AsyncException(..))
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Concurrent.Lifted
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Either
import Network.DNS
import Network.Connection
import Control.Monad.Logger
import Text.InterpolatedString.Perl6 (qq)
import Data.Default.Class
import System.Log.FastLogger (fromLogStr)
import qualified System.Console.Haskeline as HL

import Network.XMPP.Connection
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Roster
import Network.XMPP.Address
import Network.XMPP.Subscription
import Network.XMPP.Language
import Network.XMPP.Presence
import Network.XMPP.Presence.Myself
import Network.XMPP.Presence.Roster
import Network.XMPP.Message
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.MUC
import Network.XMPP.XEP.Version
import Network.XMPP.XEP.EntityTime
import Network.SASL

data Settings = Settings { server :: Text
                         , user :: Text
                         , password :: Text
                         , resource :: Text
                         , rosterCache :: FilePath
                         , logFile :: FilePath
                         }
                deriving (Show, Eq, Generic)

instance JSON.FromJSON Settings where

main :: IO ()
main = do
  mainTid <- myThreadId
  let criticalThread :: (MonadBase IO m, MonadMask m) => m () -> (forall a. m a -> m a) -> m ()
      criticalThread run unmask = unmask run `catches`
                                  [ Handler (\case ThreadKilled -> return (); e -> throwTo mainTid e)
                                  , Handler (\(e :: SomeException) -> throwTo mainTid e)
                                  ]

  [settingsFile] <- liftIO getArgs
  Just settings <- liftIO $ Yaml.decodeFile settingsFile
  logChanFile <- newChan
  logChanTerm <- dupChan logChanFile
  consoleChan <- newChan
  let writeMessage (str :: String) = writeChan consoleChan str
  let logMessage _ _ _ msg = writeChan consoleChan $ B.unpack $ fromLogStr msg
      logMessageThread = forever $ flip runLoggingT logMessage $ filterLogger (\_ lvl -> lvl > LevelInfo) $ unChanLoggingT logChanTerm

      fileLogThread = runFileLoggingT (logFile settings) $ unChanLoggingT logChanFile

  withAsyncWithUnmask (criticalThread fileLogThread) $ \_ -> withAsyncWithUnmask (criticalThread logMessageThread) $ \_ -> runChanLoggingT logChanFile $ do
    rs <- liftIO $ makeResolvSeed defaultResolvConf
    Right svrs <- liftIO $ withResolver rs $ \resolver -> runEitherT $ findServers resolver (T.encodeUtf8 $ server settings) Nothing
    $(logInfo) [qq|Found servers: $svrs|]
    cctx <- liftIO initConnectionContext
    let (host, port) = head svrs
        tsettings = TLSSettingsSimple { settingDisableCertificateValidation = False
                                      , settingDisableSession = False
                                      , settingUseServerName = True
                                      }
        esettings = ConnectionParams { connectionHostname = B.unpack host
                                    , connectionPort = fromIntegral port
                                    , connectionUseSecure = Just tsettings
                                    , connectionUseSocks = Nothing
                                    }
        csettings = ConnectionSettings { connectionParams = esettings
                                      , connectionContext = cctx
                                      , connectionServer = server settings
                                      , connectionUser = user settings
                                      , connectionAuth = [plainAuth "" (T.encodeUtf8 $ user settings) (T.encodeUtf8 $ password settings)]
                                      }
        ssettings = SessionSettings { ssConn = csettings
                                    , ssResource = resource settings
                                    }
        initMain = do
          ms <- sessionCreate ssettings
          case ms of
            Left e -> fail [qq|Error creating session: $e|]
            Right s -> stanzaSessionCreate s

    bracket initMain (sessionKill . ssSession) $ \sess -> do
      let periodicThread = forever $ threadDelay 5000000 >> sessionPeriodic (ssSession sess)

      withAsyncWithUnmask (criticalThread periodicThread) $ \periodicId -> do
        let terminate = do
              cancel periodicId
              sessionClose $ ssSession sess

        oldRoster <- liftIO $ (JSON.decodeStrict <$> B.readFile (rosterCache settings)) `catch` (\(SomeException _) -> return Nothing)
        (rosterP, rosterRef) <- rosterPlugin sess oldRoster
        (subscrP, subscrRef) <- subscriptionPlugin sess
        (rpresH, rpresRef) <- rpresencePlugin rosterRef
        (myPresH, myPresRef) <- myPresencePlugin sess
        (imP, imRef) <- imPlugin sess
        (mucP, mucPresH, mucDiscoH, mucRef) <- mucPlugin sess
        presP <- presencePlugin [rpresH, myPresH, mucPresH]
        (verP, verDiscoH) <- versionPlugin def
        (timeP, timeDiscoH) <- entityTimePlugin
        discoP <- discoPlugin [mucDiscoH, verDiscoH, timeDiscoH]

        let saveRoster = do
              roster <- rosterTryGet rosterRef
              case roster of
                Just r | Just _ <- rosterVersion r -> liftIO $ BL.writeFile (rosterCache settings) $ JSON.encode r
                _ -> return ()

        flip finally saveRoster $ do
          rosterSetHandler rosterRef $ \(_, event) -> do
            writeMessage [qq|Got roster update: $event|]

          subscriptionSetHandler subscrRef $ \(addr, stat) -> do
            writeMessage [qq|Got subscription update for $addr: $stat|]

          myPresenceSetHandler myPresRef $ \event -> do
            writeMessage [qq|Got presence update for myself: $event|]

          rpresenceSetHandler rpresRef $ \event -> do
            writeMessage [qq|Got presence update for roster: $event|]

          imSetHandler imRef $ \(addr, msg) -> do
            let text = localizedGet (Just "en") $ imBody msg
            writeMessage [qq|{addressToText addr}: $text|]

          mucSetHandler mucRef $ \event -> do
            writeMessage [qq|Got MUC event: $event|]

          myPresenceSend myPresRef $ Just def

          let plugins = [rosterP, subscrP, presP, imP, discoP, mucP, verP, timeP]

              promptLoop :: (forall a. LoggingT IO a -> HL.InputT IO a) -> HL.InputT IO ()
              promptLoop runInBase = HL.getInputLine "> " >>= \case
                Nothing -> return ()
                Just "quit" -> return ()
                Just cmd -> do
                  HL.handle (\(e :: SomeException) -> HL.outputStrLn [qq|Error while executing command: $e|]) $ case words cmd of
                    ["subscribe_from", (xmppAddress . T.pack -> Right (bareJidGet -> Just addr)), (read -> should)] -> do
                      runInBase $ updateSubscriptionFrom subscrRef addr should
                    ["subscribe_to", (xmppAddress . T.pack -> Right (bareJidGet -> Just addr)), (read -> should)] -> do
                      runInBase $ requestSubscriptionTo subscrRef addr should
                    "msg" : (xmppAddress . T.pack -> Right addr) : msg -> do
                      let imsg = plainIMMessage $ T.pack $ unwords msg
                      runInBase $ imSend imRef addr imsg
                    ["roster"] -> do
                      roster <- runInBase $ rosterGet rosterRef
                      HL.outputStrLn $ show roster
                    ["my_presence"] -> do
                      pres <- runInBase $ myPresenceGet myPresRef
                      HL.outputStrLn $ show pres
                    ["roster_presence"] -> do
                      pres <- runInBase $ rpresenceGet rpresRef
                      HL.outputStrLn $ show pres
                    ["set_presence", (read -> online)] -> do
                      runInBase $ myPresenceSend myPresRef $ if online then Just def else Nothing
                    ["roster_insert", (xmppAddress . T.pack -> Right addr)] -> do
                      runInBase $ insertRoster rosterRef addr Nothing S.empty
                    ["roster_delete", (xmppAddress . T.pack -> Right addr)] -> do
                      runInBase $ deleteRoster rosterRef addr
                    ["disco", (xmppAddress . T.pack -> Right addr)] -> do
                      topo <- runInBase $ getDiscoTopo sess addr Nothing
                      case topo of
                        Left e -> HL.outputStrLn [qq|Failed to perform discovery: $e|]
                        Right r -> HL.outputStrLn $ show r
                    ["version", (xmppAddress . T.pack -> Right addr)] -> do
                      ver <- runInBase $ getVersion sess addr
                      HL.outputStrLn $ show ver
                    ["time", (xmppAddress . T.pack -> Right addr)] -> do
                      time <- runInBase $ getEntityTime sess addr
                      HL.outputStrLn $ show time
                    ["muc_join", (xmppAddress . T.pack -> Right (fullJidGet -> Just addr))] -> do
                      runInBase $ mucJoin mucRef addr def $ \muc event -> do
                        writeMessage [qq|{bareJidToText $ fullBare addr} event: $event|]
                    "mmsg" : (xmppAddress . T.pack -> Right addr) : msg -> do
                      let imsg = (plainIMMessage $ T.pack $ unwords msg) { imType = MessageGroupchat }
                      runInBase $ imSend imRef addr imsg
                    ["muc_leave", (xmppAddress . T.pack -> Right (bareJidGet -> Just addr))] -> do
                      runInBase $ mucSendPresence mucRef addr Nothing
                    [] -> return ()
                    _ -> HL.outputStrLn "Unknown command"
                  promptLoop runInBase

              promptThread = control $ \runInBase -> HL.runInputT HL.defaultSettings $ do
                printFunc <- HL.getExternalPrint
                let writeThread = forever (fmap (++ "\n") (readChan consoleChan) >>= printFunc)
                HL.bracket (liftIO $ forkWithUnmask $ criticalThread writeThread) (liftIO . killThread) $ \_ -> do
                  promptLoop (liftIO . runInBase)

          withAsyncWithUnmask (\unmask -> unmask promptThread `finally` terminate) $ \_ -> do
            let checkClosed e@ConnectionClosedException = do
                  closed <- sessionIsClosed $ ssSession sess
                  unless closed $ throwM e
                checkClosed e = throwM e
            handle checkClosed $ forever $ stanzaSessionStep sess (pluginsInHandler plugins) (pluginsRequestIqHandler plugins)
