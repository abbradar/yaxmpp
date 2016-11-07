import Data.Maybe
import Control.Monad
import System.Environment
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent.Lifted
import Control.Monad.Trans.Either
import Network.DNS
import Network.Connection
import Control.Monad.Logger
import Text.InterpolatedString.Perl6 (qq)

import Network.XMPP.Connection
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Roster
import Network.XMPP.Disco
import Network.XMPP.Address
import Network.SASL

data Settings = Settings { server :: Text
                         , user :: Text
                         , password :: Text
                         , resource :: Text
                         , rosterCache :: FilePath
                         , pal :: XMPPAddress
                         }
                deriving (Show, Eq, Generic)

instance JSON.FromJSON Settings where

main :: IO ()
main = runStderrLoggingT $ do
  settingsFile:_ <- liftIO getArgs
  Just settings <- liftIO $ Yaml.decodeFile settingsFile
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

  bracket initMain (sessionClose . ssSession) $ \sess ->
    bracket (fork $ forever $ threadDelay 5000000 >> sessionPeriodic (ssSession sess)) killThread $ \_ -> do
      $(logInfo) "Session successfully created!"
      oldRoster <- liftIO $ (JSON.decodeStrict <$> B.readFile (rosterCache settings)) `catch` (\(SomeException _) -> return Nothing)
      (rosterP, rosterRef) <- rosterPlugin oldRoster sess

      let saveRoster = do
            roster <- tryGetRoster rosterRef
            case roster of
              Just r | Just _ <- rosterVersion r -> liftIO $ BL.writeFile (rosterCache settings) $ JSON.encode r
              _ -> return ()

      flip finally saveRoster $ do
        subscribeRoster rosterRef $ \roster -> do
          $(logInfo) [qq|Got roster update: $roster|]

        _ <- fork $ do
          let entry = RosterEntry { rentryName = Just "Best pal"
                                  , rentrySubscription = SubNone
                                  , rentryGroups = S.fromList [ "Pals" ]
                                  }
          insertRoster (pal settings) entry rosterRef

        _ <- fork $ do
          topo <- getDiscoTopo sess (fromJust $ readXMPPAddress $ server settings) Nothing
          case topo of
            Left e -> $(logWarn) [qq|Failed to perform discovery on {server settings}: $e|]
            Right r -> $(logDebug) [qq|Discovery result: $r|]

        let plugins = [rosterP]
        forever $ stanzaSessionStep sess (pluginsInHandler plugins) (pluginsRequestIqHandler plugins)
