import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent.Lifted
import Control.Monad.Trans.Either
import Network.DNS
import Network.Connection
import Control.Monad.Logger

import Network.XMPP.Connection
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.SASL

main :: IO ()
main = runStderrLoggingT $ do
  rs <- liftIO $ makeResolvSeed defaultResolvConf
  Right svrs <- liftIO $ withResolver rs $ \resolver -> runEitherT $ findServers resolver "fmap.me" Nothing
  $(logInfo) $ "Found servers: " <> T.pack (show svrs)
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
                                     , connectionServer = "fmap.me"
                                     , connectionUser = "test"
                                     , connectionAuth = [plainAuth "" "test" "test"]
                                     }
      ssettings = SessionSettings { ssConn = csettings
                                  , ssResource = "yaxmpp"
                                  }
      initSession = do
        ms <- createSession ssettings myHandler
        case ms of
          Left e -> fail $ "Error creating session: " ++ show e
          Right s -> return s

      myHandler sess e = do
        $(logInfo) $ "Stanza received by handler"
        
  bracket initSession closeSession $ \sess ->
    bracket (fork $ forever $ threadDelay 5000000 >> cleanupPending sess) killThread $ \_ -> do
      $(logInfo) "Session successfully created!"
      forever $ sessionStep sess
