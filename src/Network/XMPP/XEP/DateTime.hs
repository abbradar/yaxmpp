module Network.XMPP.XEP.DateTime
  ( xmppDate
  , xmppTime
  , xmppDateTime

  , dayToXmpp
  , timeOfDayToXmpp
  , zonedTimeToXmpp
  , utcTimeToXmpp
  ) where

import Prelude hiding (take)
import Data.Text (Text)
import qualified Data.Text.Read as TR
import Data.Int
import Data.Maybe
import Data.Fixed
import Data.Semigroup
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Attoparsec.Text
import TextShow
import TextShow.Data.Integral

fixedDecimal :: Integral a => Int -> Parser a
fixedDecimal n = do
  str <- take n
  case TR.decimal str of
    Left err -> fail err
    Right (a, "") -> return a
    Right (_, _) -> fail "fixedDecimal: leftovers"

-- Expects non-negative numbers.
showbPaddedDecimal :: Integral a => Int64 -> a -> Builder
showbPaddedDecimal n int = mtimesDefault (max 0 (n - lengthB str)) (singleton '0') <> str
  where str = showbIntegralPrec 0 int

xmppDate :: Parser Day
xmppDate = do
  year <- fixedDecimal 4
  _ <- char '-'
  month <- fixedDecimal 2
  _ <- char '-'
  day <- fixedDecimal 2
  case fromGregorianValid year month day of
    Just r -> return r
    Nothing -> fail "xmppDate: invalid Gregorian date"

xmppTZ :: Parser TimeZone
xmppTZ = do
  signC <- anyChar >>= \case
    '+' -> return 1
    '-' -> return (-1)
    _ -> fail "xmppTZ: invalid timezone sign"
  hour <- fixedDecimal 2
  _ <- char ':'
  minute <- fixedDecimal 2
  return $ minutesToTimeZone $ signC * (60 * hour + minute)

xmppTime :: Parser (TimeZone, TimeOfDay)
xmppTime = do
  hour <- fixedDecimal 2
  _ <- char ':'
  minute <- fixedDecimal 2
  _ <- char ':'
  (sec :: Int) <- fixedDecimal 2
  (millis :: Milli) <- fmap (fromMaybe 0) $ optional $ do
    _ <- char '.'
    MkFixed <$> fixedDecimal 3
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

dayToXmpp' :: Day -> Builder
dayToXmpp' (toGregorian -> (year, month, day)) =
  showbPaddedDecimal 4 year
  <> singleton '-' <> showbPaddedDecimal 2 month
  <> singleton '-' <> showbPaddedDecimal 2 day

dayToXmpp :: Day -> Text
dayToXmpp = toText . dayToXmpp'

timeOfDayToXmpp' :: TimeZone -> TimeOfDay -> Builder
timeOfDayToXmpp' tz (TimeOfDay {..}) =
  showbPaddedDecimal 2 todHour
  <> singleton ':' <> showbPaddedDecimal 2 todMin
  <> singleton ':' <> showbPaddedDecimal 2 todRSec
  <> singleton '.' <> showbPaddedDecimal 3 todRMsec
  <> tzStr
  where MkFixed msec = fromRational $ toRational todSec :: Milli
        (todRSec, todRMsec) = msec `divMod` 1000
        tzStr
          | tz == utc = singleton 'Z'
          | otherwise =
            singleton tzSgn <> showbPaddedDecimal 2 tzHour <> singleton ':' <> showbPaddedDecimal 2 tzMin
        tzSgn
          | timeZoneMinutes tz < 0 = '-'
          | otherwise = '+'
        (tzHour, tzMin) = (abs $ timeZoneMinutes tz) `divMod` 60

timeOfDayToXmpp :: TimeZone -> TimeOfDay -> Text
timeOfDayToXmpp tz tod = toText $ timeOfDayToXmpp' tz tod

zonedTimeToXmpp' :: ZonedTime -> Builder
zonedTimeToXmpp' (ZonedTime (LocalTime date time) tz) =
  dayToXmpp' date <> singleton 'T' <> timeOfDayToXmpp' tz time

zonedTimeToXmpp :: ZonedTime -> Text
zonedTimeToXmpp = toText . zonedTimeToXmpp'

utcTimeToXmpp :: UTCTime -> Text
utcTimeToXmpp t = zonedTimeToXmpp $ utcToZonedTime utc t
