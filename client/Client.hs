import Control.Monad
import System.Environment
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef.Lifted
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent.Lifted
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Either
import Network.DNS
import Network.Connection
import Control.Monad.Logger
import Text.InterpolatedString.Perl6 (qq)
import Data.Default.Class
import qualified System.Console.Haskeline as HL

import Network.XMPP.Connection
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Roster
import Network.XMPP.Address
import Network.XMPP.Subscription
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
                         , pal :: BareJID
                         , conference :: FullJID
                         , logFile :: FilePath
                         }
                deriving (Show, Eq, Generic)

instance JSON.FromJSON Settings where

main :: IO ()
main = do
  settingsFile:_ <- liftIO getArgs
  Just settings <- liftIO $ Yaml.decodeFile settingsFile
  runFileLoggingT (logFile settings) $ do
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
      let periodicThread unmask =
            unmask (forever $ threadDelay 5000000 >> sessionPeriodic (ssSession sess))
            `finally`
            sessionClose (ssSession sess)

      withAsyncWithUnmask periodicThread $ \periodicId -> do
        let terminate = cancel periodicId
        consoleChan <- newChan
        consoleChanClosed <- newIORef False
        let writeMessage (str :: String) = do
              c <- readIORef consoleChanClosed
              when c $ fail "writeMessage: channel is closed"
              writeChan consoleChan str

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

          subscriptionSetHandler subscrRef $ \(addr, stat) -> void $ fork $ do
            writeMessage [qq|Got subscription update for $addr: $stat|]
            case stat of
              TheyRequested -> updateSubscriptionFrom subscrRef addr True
              TheyUnsubscribed -> do
                -- XXX: Prosody has a bug when it sends "unsubscribed" events without according subscription request.
                let fullAddr = bareJidAddress addr
                roster <- rosterGet rosterRef
                when (fullAddr `M.member` rosterEntries roster) $ deleteRoster rosterRef $ bareJidAddress addr
              _ -> return ()

          myPresenceSetHandler myPresRef $ \event -> do
            writeMessage [qq|Got presence update for myself: $event|]

          rpresenceSetHandler rpresRef $ \event -> do
            writeMessage [qq|Got presence update for roster: $event|]

          imSetHandler imRef $ \(addr, msg) -> void $ fork $ do
            ver <- getVersion sess addr
            time <- getEntityTime sess addr
            writeMessage [qq|Got message from $addr, version: $ver, time $time|]
            case imType msg of
              MessageGroupchat -> imSend imRef (addressBare addr) (msg { imExtended = [] })
              _ -> imSend imRef addr (msg { imExtended = [] })

          mucSetHandler mucRef $ \event -> do
            writeMessage [qq|Got MUC event: $event|]

          _ <- fork $ flip onException terminate $ do
            rst <- rosterGet rosterRef
            writeMessage [qq|Got initial roster: $rst|]
            myPresenceSend myPresRef $ Just def
            -- insertRoster rosterRef (bareJidAddress $ pal settings) (Just "Best pal") (S.fromList ["Pals"])
            -- requestSubscription subscrRef $ pal settings
            -- mucJoin mucRef (conference settings) def $ \_ event -> do
            --   $(logInfo) [qq|Got event from MUC room that I joined: $event|]

          -- _ <- fork $ do
          --   topo <- getDiscoTopo sess (fromJust $ readXMPPAddress $ server settings) Nothing
          --   case topo of
          --     Left e -> $(logWarn) [qq|Failed to perform discovery on {server settings}: $e|]
          --     Right r -> $(logDebug) [qq|Discovery result: $r|]

          let plugins = [rosterP, subscrP, presP, imP, discoP, mucP, verP, timeP]

          let promptLoop = HL.getInputLine "> " >>= \case
                Nothing -> return ()
                Just "quit" -> return ()
                Just _ -> promptLoop

              promptThread = HL.runInputT HL.defaultSettings $ do
                printFunc <- HL.getExternalPrint
                let writeThread = forever (fmap (++ "\n") (readChan consoleChan) >>= printFunc)
                HL.bracket (liftIO $ forkFinally writeThread (\_ -> atomicWriteIORef consoleChanClosed True)) (liftIO . killThread) $ \_ -> do
                  HL.outputStrLn "Client started"
                  promptLoop

          withAsyncWithUnmask (\unmask -> unmask (liftIO promptThread) `finally` terminate) $ \_ -> do
            let checkClosed e@ConnectionClosedException = do
                  closed <- sessionIsClosed $ ssSession sess
                  unless closed $ throwM e
                checkClosed e = throwM e
            handle checkClosed $ forever $ stanzaSessionStep sess (pluginsInHandler plugins) (pluginsRequestIqHandler plugins)
