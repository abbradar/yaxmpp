module Data.Time.XMPP
  ( xmppDay
  , xmppTimeOfDay
  , xmppTimeZone
  , xmppZonedTime

  , dayToXmpp
  , timeOfDayToXmpp
  , timeZoneToXmpp
  , zonedTimeToXmpp

  , utcTimeToXmpp
  ) where

import Prelude hiding (take)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import Data.Attoparsec.Time

import Network.XMPP.Utils

--
-- Parsers
--

xmppDay :: Text -> Either String Day
xmppDay = parseValue day

xmppTimeZone :: Text -> Either String TimeZone
xmppTimeZone = parseValue $ fmap (fromMaybe utc) timeZone

xmppTimeOfDay :: Text -> Either String TimeOfDay
xmppTimeOfDay = parseValue timeOfDay

xmppZonedTime :: Text -> Either String ZonedTime
xmppZonedTime = parseValue zonedTime

--
-- Renderers
--

dayToXmpp :: Day -> Text
dayToXmpp = T.pack . iso8601Show

timeOfDayToXmpp :: TimeOfDay -> Text
timeOfDayToXmpp = T.pack . iso8601Show

timeZoneToXmpp :: TimeZone -> Text
timeZoneToXmpp = T.pack . iso8601Show

zonedTimeToXmpp :: ZonedTime -> Text
zonedTimeToXmpp = T.pack . iso8601Show

utcTimeToXmpp :: UTCTime -> Text
utcTimeToXmpp = T.pack . iso8601Show
