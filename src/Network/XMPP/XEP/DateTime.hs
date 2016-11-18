module Network.XMPP.XEP.DateTime
  ( xmppDate
  , xmppTime
  , xmppDateTime

  , dayToXmpp
  , timeOfDayToXmpp
  , utcTimeToXmpp
  , zonedTimeToXmpp
  ) where

import Data.Text (Text)
import Data.Maybe
import Data.Fixed
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Attoparsec.Text
import Text.InterpolatedString.Perl6 (qq)

xmppDate :: Parser Day
xmppDate = do
  year <- decimal
  _ <- char '-'
  month <- decimal
  _ <- char '-'
  day <- decimal
  case fromGregorianValid year month day of
    Just r -> return r
    Nothing -> fail "xmppDate: invalid Gregorian date"

xmppTZ :: Parser TimeZone
xmppTZ = do
  signC <- anyChar >>= \case
    '+' -> return 1
    '-' -> return (-1)
    _ -> fail "xmppTZ: invalid timezone sign"
  hour <- decimal
  _ <- char ':'
  minute <- decimal
  return $ minutesToTimeZone $ signC * (60 * hour + minute)

xmppTime :: Parser (TimeZone, TimeOfDay)
xmppTime = do
  hour <- decimal
  _ <- char ':'
  minute <- decimal
  _ <- char ':'
  (sec :: Int) <- decimal
  (millis :: Milli) <- fmap (fromMaybe 0) $ optional $ do
    _ <- char '.'
    MkFixed <$> decimal
  time <- case makeTimeOfDayValid hour minute (fromIntegral sec + fromRational (toRational millis)) of
    Just t -> return t
    Nothing -> fail "xmppTime: invalid time"
  tz <- (utc <$ char 'Z') <|> xmppTZ
  return (tz, time)

xmppDateTime :: Parser ZonedTime
xmppDateTime = do
  date <- xmppDate
  _ <- char 'T'
  (tz, time) <- xmppTime
  return $ ZonedTime (LocalTime date time) tz

dayToXmpp :: Day -> Text
dayToXmpp (toGregorian -> (year, month, day)) = [qq|{year}-{month}-{day}|]

timeOfDayToXmpp :: TimeZone -> TimeOfDay -> Text
timeOfDayToXmpp tz (TimeOfDay {..}) = [qq|{todHour}:{todMin}:{sec}{tzStr}|]
  where sec = fromRational $ toRational todSec :: Milli
        tzStr :: Text
        tzStr
          | tz == utc = "Z"
          | otherwise = [qq|{tzSgn}{tzHour}:{tzMin}|]
        tzSgn :: Text
        tzSgn
          | timeZoneMinutes tz < 0 = "-"
          | otherwise = "+"
        (tzHour, tzMin) = (abs $ timeZoneMinutes tz) `divMod` 60

utcTimeToXmpp :: UTCTime -> Text
utcTimeToXmpp t = zonedTimeToXmpp $ ZonedTime (utcToLocalTime utc t) utc

zonedTimeToXmpp :: ZonedTime -> Text
zonedTimeToXmpp (ZonedTime (LocalTime date time) tz) = [qq|{dayToXmpp date}T{timeOfDayToXmpp tz time}|]
