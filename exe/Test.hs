import Control.Monad
import System.Environment
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
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
import Network.SASL

data Settings = Settings { server :: ByteString
                         , user :: Text
                         , password :: Text
                         , resource :: Text
                         }
                deriving (Show, Read, Eq)

main :: IO ()
main = runStderrLoggingT $ do
  settingsfile:_ <- liftIO getArgs
  settings <- liftIO $ read <$> readFile settingsfile
  rs <- liftIO $ makeResolvSeed defaultResolvConf
  Right svrs <- liftIO $ withResolver rs $ \resolver -> runEitherT $ findServers resolver (server settings) Nothing
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
                                     , connectionServer = T.decodeUtf8 $ server settings
                                     , connectionUser = user settings
                                     , connectionAuth = [plainAuth "" (T.encodeUtf8 $ user settings) (T.encodeUtf8 $ password settings)]
                                     }
      ssettings = SessionSettings { ssConn = csettings
                                  , ssResource = resource settings
                                  }
      initMain = do
        ms <- stanzaSessionCreate ssettings
        case ms of
          Left e -> fail [qq|Error creating session: $e|]
          Right s -> return s

      msgHandler msg = do
        $(logDebug) [qq|Stanza received: $msg|]
        return Nothing

      iqHandler iq = do
        $(logDebug) [qq|Request received: $iq|]
        return $ Left $ featureNotImplemented "Not implemented"
          
  bracket initMain stanzaSessionClose $ \sess ->
    bracket (fork $ forever $ threadDelay 5000000 >> stanzaSessionPeriodic sess) killThread $ \_ -> do
      $(logInfo) "Session successfully created!"
      forever $ stanzaSessionStep sess msgHandler iqHandler
