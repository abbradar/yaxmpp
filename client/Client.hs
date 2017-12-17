import Control.Monad
import System.Environment
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
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

data Command = Command { commandHandler :: (forall a. LoggingT IO a -> HL.InputT IO a) -> [String] -> HL.InputT IO ()
                       , commandAutocomplete :: [String] -> String -> IO [HL.Completion]
                       }

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

              commands = M.fromListWith (\_ _ -> error "Repeating command definitions")
                [ ( "subscribe_from"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right (bareJidGet -> Just addr)), (read -> should)] -> do
                                  runInBase $ updateSubscriptionFrom subscrRef addr should
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "subscribe_to"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right (bareJidGet -> Just addr)), (read -> should)] -> do
                                  runInBase $ requestSubscriptionTo subscrRef addr should
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "msg"
                  , Command { commandHandler = \runInBase args -> case args of
                                (xmppAddress . T.pack -> Right addr) : msg -> do
                                  let imsg = plainIMMessage $ T.pack $ unwords msg
                                  runInBase $ imSend imRef addr imsg
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "roster"
                  , Command { commandHandler = \runInBase args -> case args of
                                [] -> do
                                  roster <- runInBase $ rosterGet rosterRef
                                  HL.outputStrLn $ show roster
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "my_presence"
                  , Command { commandHandler = \runInBase args -> case args of
                                [] -> do
                                  pres <- runInBase $ myPresenceGet myPresRef
                                  HL.outputStrLn $ show pres
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "roster_presence"
                  , Command { commandHandler = \runInBase args -> case args of
                                [] -> do
                                  pres <- runInBase $ rpresenceGet rpresRef
                                  HL.outputStrLn $ show pres
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "set_presence"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(read -> online)] -> do
                                  runInBase $ myPresenceSend myPresRef $ if online then Just def else Nothing
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "roster_insert"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right addr)] -> do
                                  runInBase $ insertRoster rosterRef addr Nothing S.empty
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "roster_delete"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right addr)] -> do
                                  runInBase $ deleteRoster rosterRef addr
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "disco"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right addr)] -> do
                                  topo <- runInBase $ getDiscoTopo sess addr Nothing
                                  case topo of
                                    Left e -> HL.outputStrLn [qq|Failed to perform discovery: $e|]
                                    Right r -> HL.outputStrLn $ show r
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "version"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right addr)] -> do
                                  ver <- runInBase $ getVersion sess addr
                                  HL.outputStrLn $ show ver
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "time"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right addr)] -> do
                                  time <- runInBase $ getEntityTime sess addr
                                  HL.outputStrLn $ show time
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "muc_join"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right (fullJidGet -> Just addr))] -> do
                                  runInBase $ mucJoin mucRef addr def $ \_ event -> do
                                    writeMessage [qq|{bareJidToText $ fullBare addr} event: $event|]
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "mmsg"
                  , Command { commandHandler = \runInBase args -> case args of
                                (xmppAddress . T.pack -> Right addr) : msg -> do
                                  let imsg = (plainIMMessage $ T.pack $ unwords msg) { imType = MessageGroupchat }
                                  runInBase $ imSend imRef addr imsg
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                , ( "muc_leave"
                  , Command { commandHandler = \runInBase args -> case args of
                                [(xmppAddress . T.pack -> Right (bareJidGet -> Just addr))] -> do
                                  runInBase $ mucSendPresence mucRef addr Nothing
                                _ -> HL.outputStrLn "Invalid arguments"
                            , commandAutocomplete = \_ _ -> return []
                            }
                  )
                ]

              inputCompletion = HL.completeWordWithPrev Nothing [' '] $ \prevArgs word -> do
                case words prevArgs of
                  [] -> return $ map HL.simpleCompletion $ filter (word `isPrefixOf`) $ M.keys commands
                  cmd : args ->
                    case M.lookup cmd commands of
                      Nothing -> return []
                      Just handler -> commandAutocomplete handler args word
              inputSettings = HL.setComplete inputCompletion HL.defaultSettings

              promptLoop :: (forall a. LoggingT IO a -> IO a) -> HL.InputT IO ()
              promptLoop runInBase = HL.getInputLine "> " >>= \case
                Nothing -> return ()
                Just cmdStr -> case words cmdStr of
                  [] -> promptLoop runInBase
                  ["quit"] -> return ()
                  cmd : args -> case M.lookup cmd commands of
                    Just handler -> do
                      HL.handle (\(e :: SomeException) -> HL.outputStrLn [qq|Error while executing command: $e|]) $ commandHandler handler (liftIO . runInBase) args
                      promptLoop runInBase
                    Nothing -> HL.outputStrLn "Unknown command"

              promptThread = control $ \runInBase -> HL.runInputT inputSettings $ do
                printFunc <- HL.getExternalPrint
                let writeThread = forever (fmap (++ "\n") (readChan consoleChan) >>= printFunc)
                HL.bracket (liftIO $ forkWithUnmask $ criticalThread writeThread) (liftIO . killThread) $ \_ -> do
                  promptLoop runInBase

          withAsyncWithUnmask (\unmask -> unmask promptThread `finally` terminate) $ \_ -> do
            let checkClosed e@ConnectionClosedException = do
                  closed <- sessionIsClosed $ ssSession sess
                  unless closed $ throwM e
                checkClosed e = throwM e
            handle checkClosed $ forever $ stanzaSessionStep sess (pluginsInHandler plugins) (pluginsRequestIqHandler plugins)
