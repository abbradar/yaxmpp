import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Slot (SlotSignal (..))
import qualified Control.Slot as Slot
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Network.Connection
import Network.DNS
import qualified System.Console.Haskeline as HL
import System.Environment
import Control.Concurrent.Linked
import UnliftIO (liftIO, withRunInIO)
import UnliftIO.Concurrent

import qualified Data.Registry.Mutable as RegRef
import Network.SASL
import Network.XMPP.Address
import Network.XMPP.Connection
import Network.XMPP.Language
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Presence.Myself
import Network.XMPP.Presence.Roster
import Network.XMPP.Roster
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.Subscription
import Network.XMPP.XEP.Capabilities
import Network.XMPP.XEP.ChatStates
import Network.XMPP.XEP.DelayedDelivery
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.EntityTime
import Network.XMPP.XEP.MUC
import Network.XMPP.XEP.Version

newtype ClientPlugin m = ClientPlugin {clientWriteMessage :: String -> m ()}

instance (MonadStream m) => SlotSignal m (RosterEntries, RosterEvent) (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) (_, event) =
    clientWriteMessage [i|Got roster update: #{event}|]

instance (MonadStream m) => SlotSignal m (BareJID, SubscriptionStatus) (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) (addr, stat) =
    clientWriteMessage [i|Got subscription update for #{addr}: #{stat}|]

instance (MonadStream m) => SlotSignal m (PresenceEvent XMPPResource) (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) event =
    clientWriteMessage [i|Got presence update for myself: #{event}|]

instance (MonadStream m) => SlotSignal m RosterPresenceEvent (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) event =
    clientWriteMessage [i|Got presence update for roster: #{event}|]

instance (MonadStream m) => SlotSignal m (XMPPAddress, IMMessage) (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) (addr, msg) = do
    let text = localizedGet (Just "en") $ imBody msg
    clientWriteMessage [i|#{addressToText addr}: #{text}|]

instance (MonadStream m) => SlotSignal m MUCEvent (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) event =
    clientWriteMessage [i|Got MUC event: #{event}|]

instance (MonadStream m) => SlotSignal m (XMPPAddress, MessageType, ChatState) (ClientPlugin m) where
  emitSignal (ClientPlugin {..}) (addr, _msgType, cs) =
    clientWriteMessage [i|#{addressToText addr} is #{cs}|]

data Settings = Settings
  { server :: Text
  , user :: Text
  , password :: Text
  , resource :: Text
  , cachePath :: FilePath
  , logFile :: FilePath
  }
  deriving (Show, Eq, Generic)

data Command = Command
  { commandHandler :: (forall a. LoggingT IO a -> HL.InputT IO a) -> [String] -> HL.InputT IO ()
  , commandAutocomplete :: [String] -> String -> IO [HL.Completion]
  }

instance JSON.FromJSON Settings

main :: IO ()
main = do
  [settingsFile] <- getArgs
  Right settings <- Yaml.decodeFileEither settingsFile
  logChanFile <- newChan
  logChanTerm <- dupChan logChanFile
  consoleChan <- newChan
  let writeMessage (str :: String) = writeChan consoleChan str
  let logMessage _ _ _ msg = writeChan consoleChan $ B.unpack $ fromLogStr msg
      logMessageThread = forever $ flip runLoggingT logMessage $ filterLogger (\_ lvl -> lvl > LevelInfo) $ unChanLoggingT logChanTerm

      fileLogThread = runFileLoggingT (logFile settings) $ unChanLoggingT logChanFile

  bracket (forkLinked fileLogThread) killThread $ \_ -> bracket (forkLinked logMessageThread) killThread $ \_ -> runChanLoggingT logChanFile $ do
    rs <- liftIO $ makeResolvSeed defaultResolvConf
    Right svrs <- liftIO $ withResolver rs $ \resolver -> runExceptT $ findServers resolver (T.encodeUtf8 $ server settings) Nothing
    $(logInfo) [i|Found servers: #{svrs}|]
    cctx <- liftIO initConnectionContext
    (host, port) : _ <- pure svrs
    let tsettings =
          TLSSettingsSimple
            { settingDisableCertificateValidation = False
            , settingDisableSession = False
            , settingUseServerName = True
            , settingClientSupported = def
            }
        esettings =
          ConnectionParams
            { connectionHostname = B.unpack host
            , connectionPort = fromIntegral port
            , connectionUseSecure = Just tsettings
            , connectionUseSocks = Nothing
            }
        csettings =
          ConnectionSettings
            { connectionParams = esettings
            , connectionContext = cctx
            , connectionServer = server settings
            , connectionUser = user settings
            , connectionAuth = [plainAuth "" (T.encodeUtf8 $ user settings) (T.encodeUtf8 $ password settings)]
            }
        ssettings =
          SessionSettings
            { ssConn = csettings
            , ssResource = resource settings
            }
        initMain = do
          ms <- sessionCreate ssettings
          case ms of
            Left e -> fail [i|Error creating session: #{e}|]
            Right s -> stanzaSessionCreate s

    bracket initMain (sessionKill . ssSession) $ \sess -> do
      let periodicThread = forever $ threadDelay 5000000 >> sessionPeriodic (ssSession sess)

      bracket (forkLinked periodicThread) killThread $ \periodicId -> do
        let terminate = do
              killThread periodicId
              sessionClose $ ssSession sess

        oldCache <- liftIO $ (JSON.decodeStrict <$> B.readFile (cachePath settings)) `catch` (\(SomeException _) -> return Nothing)
        pluginsRef <- newXmppPlugins sess oldCache
        presencePlugin pluginsRef
        capsPlugin pluginsRef
        discoPlugin pluginsRef
        rosterPlugin pluginsRef
        subscriptionPlugin pluginsRef
        rpresencePlugin pluginsRef
        myPresencePlugin pluginsRef
        imPlugin pluginsRef
        delayedDeliveryPlugin pluginsRef
        mucPlugin pluginsRef
        chatStatePlugin pluginsRef
        versionPlugin pluginsRef defaultVersion
        entityTimePlugin pluginsRef

        -- Print server features
        serverFeats <- RegRef.read $ pluginsServerFeatures pluginsRef
        writeMessage [i|Parsed server features: #{serverFeats}|]

        let saveCache = do
              cache <- getCache pluginsRef
              liftIO $ BL.writeFile (cachePath settings) $ JSON.encode cache

        flip finally saveCache $ do
          let clientPlugin = ClientPlugin writeMessage

          rSlot <- rosterSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin rSlot
          sSlot <- subscriptionSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin sSlot
          mpSlot <- myPresenceSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin mpSlot
          rpSlot <- rpresenceSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin rpSlot
          imS <- imSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin imS
          mSlot <- mucSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin mSlot
          csSlot <- chatStateSlot pluginsRef
          Slot.pushNewOrFailM clientPlugin csSlot

          myPresenceSend pluginsRef $ Just defaultPresence

          let commands =
                M.fromListWith
                  (\_ _ -> error "Repeating command definitions")
                  [
                    ( "subscribe_from"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right (bareJidGet -> Just addr)), (read -> should)] -> do
                              runInBase $ updateSubscriptionFrom pluginsRef addr should
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "subscribe_to"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right (bareJidGet -> Just addr)), (read -> should)] -> do
                              runInBase $ requestSubscriptionTo pluginsRef addr should
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "msg"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            (xmppAddress . T.pack -> Right addr) : msg -> do
                              let imsg = plainIMMessage $ T.pack $ unwords msg
                              runInBase $ imSend pluginsRef addr imsg
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "roster"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [] -> do
                              roster <- runInBase $ getRoster pluginsRef
                              HL.outputStrLn $ show roster
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "my_presence"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [] -> do
                              pres <- runInBase $ myPresenceGet pluginsRef
                              HL.outputStrLn $ show pres
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "roster_presence"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [] -> do
                              pres <- runInBase $ getRosterPresence pluginsRef
                              HL.outputStrLn $ show pres
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "set_presence"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(read -> online)] -> do
                              runInBase $ myPresenceSend pluginsRef $ if online then Just defaultPresence else Nothing
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "roster_insert"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right addr)] -> do
                              runInBase $ insertRoster pluginsRef addr Nothing S.empty
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "roster_delete"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right addr)] -> do
                              runInBase $ deleteRoster pluginsRef addr
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "disco"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right addr)] ->
                              runInBase $ getDiscoTopo pluginsRef addr Nothing $ \case
                                Left e -> liftIO $ putStrLn [i|Failed to perform discovery: #{e}|]
                                Right r -> liftIO $ putStrLn $ show r
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "version"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right addr)] ->
                              runInBase $ getVersion pluginsRef addr $ \ver ->
                                liftIO $ putStrLn $ show ver
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "time"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right addr)] ->
                              runInBase $ getEntityTime pluginsRef addr $ \time ->
                                liftIO $ putStrLn $ show time
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "muc_join"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right (fullJidGet -> Just addr))] -> do
                              void $ runInBase $ mucJoin pluginsRef addr defaultMUCJoinSettings $ \_ event -> do
                                writeMessage [i|#{bareJidToText $ fullBare addr} event: #{event}|]
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "mmsg"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            (xmppAddress . T.pack -> Right addr) : msg -> do
                              let imsg = (plainIMMessage $ T.pack $ unwords msg) {imType = MessageGroupchat}
                              runInBase $ imSend pluginsRef addr imsg
                            _ -> HL.outputStrLn "Invalid arguments"
                        , commandAutocomplete = \_ _ -> return []
                        }
                    )
                  ,
                    ( "muc_leave"
                    , Command
                        { commandHandler = \runInBase args -> case args of
                            [(xmppAddress . T.pack -> Right (bareJidGet -> Just addr))] -> do
                              runInBase $ mucSendPresence pluginsRef addr Nothing
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
              promptLoop runInBase =
                HL.getInputLine "> " >>= \case
                  Nothing -> return ()
                  Just cmdStr -> case words cmdStr of
                    [] -> promptLoop runInBase
                    ["quit"] -> return ()
                    cmd : args -> do
                      case M.lookup cmd commands of
                        Just handler -> do
                          handle (\(e :: SomeException) -> HL.outputStrLn [i|Error while executing command: #{e}|]) $ commandHandler handler (liftIO . runInBase) args
                        Nothing -> HL.outputStrLn "Unknown command"
                      promptLoop runInBase

              promptThread = withRunInIO $ \runInBase -> HL.runInputT inputSettings $ do
                printFunc <- HL.getExternalPrint
                let writeThread = forever (readChan consoleChan >>= printFunc . (++ "\n"))
                bracket (liftIO $ forkLinked writeThread) (liftIO . killThread) $ \_ -> do
                  promptLoop runInBase

          bracket (forkLinked $ promptThread `finally` terminate) killThread $ \_ -> do
            let checkClosed e@ConnectionClosedException = do
                  closed <- sessionIsClosed $ ssSession sess
                  unless closed $ throwM e
                checkClosed e = throwM e
            handle checkClosed $ forever $ pluginsSessionStep pluginsRef
