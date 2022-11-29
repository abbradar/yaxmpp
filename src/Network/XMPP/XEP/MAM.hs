module Network.XMPP.XEP.MAM
  ( getEntityTime
  , entityTimePlugin
  ) where

import Data.Text (Text)
import qualified Data.Set as S
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.Time.LocalTime
import Data.Time.Clock
import Control.Monad.IO.Class

import Data.Time.XMPP
import Network.XMPP.XML
import Network.XMPP.Stream
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Address
import Network.XMPP.XEP.Disco

mamNS :: Text
mamName :: Text -> Name
(mamNS, mamName) = namePair "urn:xmpp:mam:2"

timeIqHandler :: MonadStream m => InRequestIQ -> m (Maybe (Either StanzaError [Element]))
timeIqHandler (InRequestIQ { iriType = IQGet, iriChildren = [req] })
  | elementName req == timeTag = do
      tz <- liftIO getCurrentTimeZone
      utime <- liftIO getCurrentTime
      let result =
            [ ("tzo", timeZoneToXmpp tz)
            , ("utc", utcTimeToXmpp utime)
            ]
      return $ Just $ Right [element timeTag [] $ map (\(name, value) -> NodeElement $ element (timeName name) [] [NodeContent value]) result]

  where timeTag = timeName "time"
timeIqHandler _ = return Nothing

queryArchive :: MonadStream m => StanzaSession m -> XMPPAddress -> m (Either StanzaError ZonedTime)
queryArchive sess addr = do
  ret <- stanzaSyncRequest sess OutRequestIQ { oriTo = Just addr
                                             , oriIqType = IQGet
                                             , oriChildren = [closedElement (timeName "time")]
                                             }
  return $ case ret of
    Left (e, _) -> Left e
    Right [r] | elementName r == timeName "time"
              , [tzoStr] <- getEntry r "tzo"
              , Right tz <- xmppTimeZone tzoStr
              , [utcStr] <- getEntry r "utc"
              , Right utime <- xmppZonedTime utcStr
              -- Not strictly required but is a MUST by XEP
              , zonedTimeZone utime == utc
                -> Right $ utcToZonedTime tz $ zonedTimeToUTC utime
    _ -> Left $ badRequest "getEntityTime: invalid response"

  where getEntry r name = fromElement r $/ XC.element (timeName name) &/ content

entityTimePlugin :: MonadStream m => m (XMPPPlugin m, DiscoPlugin)
entityTimePlugin = do
  let xmppPlugin = emptyPlugin { pluginRequestIqHandler = timeIqHandler
                               }
      discoHandler = emptyDiscoPlugin { discoPEntity = emptyDiscoEntity { discoFeatures = S.singleton timeNS }
                                      }
  return (xmppPlugin, discoHandler)
