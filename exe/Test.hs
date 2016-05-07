import Data.Monoid
import qualified Data.Text as T
import Control.Monad.Base
import Control.Monad.Trans.Either
import Network.DNS
import Control.Monad.Logger

import Network.XMPP.Connection
import Network.XMPP.Stream
import Network.SASL

main :: IO ()
main = runStderrLoggingT $ do
  rs <- liftBase $ makeResolvSeed defaultResolvConf
  Right svrs <- liftBase $ withResolver rs $ \resolver -> runEitherT $ findServers resolver "fmap.me" Nothing
  $(logInfo) $ "Found servers: " <> T.pack (show svrs)
  let (host, port) = head svrs
      csettings = ConnectionSettings { connectionEndpoint = tlsClientConfig port host
                                     , connectionServer = "fmap.me"
                                     , connectionUser = "test"
                                     , connectionAuth = [plainAuth "test" "test"]
                                     }
  r <- runClient csettings $ \stream -> do
    liftBase $ print (streamInfo stream, streamFeatures stream)
  case r of
    Right _ -> return ()
    Left e -> $(logError) $ "Stream creation error: " <> T.pack (show e)
