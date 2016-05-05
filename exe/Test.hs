import Control.Monad.Base
import Control.Monad.Trans.Either
import Network.DNS
import Control.Monad.Logger

import Network.XMPP.Connection
import Network.XMPP.Stream

main :: IO ()
main = do
  rs <- makeResolvSeed defaultResolvConf
  Right svrs <- withResolver rs $ \resolver -> runEitherT $ findServers resolver "fmap.me" Nothing
  print svrs
  let (host, port) = head svrs
      csettings = connectionSettings host port "fmap.me" "test"
  runStderrLoggingT $ runClient csettings $ \stream -> do
    liftBase $ print (streamInfo stream, streamFeatures stream)
