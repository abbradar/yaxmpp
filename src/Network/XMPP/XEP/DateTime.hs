module Network.XMPP.XEP.DateTime
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
import qualified Data.Text.Read as TR
import Data.Int
import Data.Maybe
import Data.Fixed
import Data.Semigroup
import Control.Applicative
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Attoparsec.Text
import TextShow
import TextShow.Data.Integral

import Network.XMPP.Utils

--
-- Helpers
--

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

--
-- Parsers
--

xmppDay' :: Parser Day
xmppDay' = do
  year <- fixedDecimal 4
  _ <- char '-'
  month <- fixedDecimal 2
  _ <- char '-'
  day <- fixedDecimal 2
  case fromGregorianValid year month day of
    Just r -> return r
    Nothing -> fail "xmppDay': invalid Gregorian date"

xmppDay :: Text -> Either String Day
xmppDay = parseValue xmppDay'

xmppTimeZone' :: Parser TimeZone
xmppTimeZone' = (utc <$ char 'Z') <|> nonUtc
  where nonUtc =do
          signC <- anyChar >>= \case
            '+' -> return 1
            '-' -> return (-1)
            _ -> fail "xmppTimeZone': invalid timezone sign"
          hour <- fixedDecimal 2
          _ <- char ':'
          minute <- fixedDecimal 2
          return $ minutesToTimeZone $ signC * (60 * hour + minute)

xmppTimeZone :: Text -> Either String TimeZone
xmppTimeZone = parseValue xmppTimeZone'

xmppTimeOfDay' :: Parser TimeOfDay
xmppTimeOfDay' = do
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
    Nothing -> fail "xmppTimeOfDay': invalid time"
  return time

xmppTimeOfDay :: Text -> Either String TimeOfDay
xmppTimeOfDay = parseValue xmppTimeOfDay'

xmppZonedTime' :: Parser ZonedTime
xmppZonedTime' = do
  date <- xmppDay'
  _ <- char 'T'
  time <- xmppTimeOfDay'
  tz <- xmppTimeZone'
  return $ ZonedTime (LocalTime date time) tz

xmppZonedTime :: Text -> Either String ZonedTime
xmppZonedTime = parseValue xmppZonedTime'

--
-- Renderers
--

dayToXmpp' :: Day -> Builder
dayToXmpp' (toGregorian -> (year, month, day)) =
  showbPaddedDecimal 4 year
  <> singleton '-' <> showbPaddedDecimal 2 month
  <> singleton '-' <> showbPaddedDecimal 2 day

dayToXmpp :: Day -> Text
dayToXmpp = toText . dayToXmpp'

timeOfDayToXmpp' :: TimeOfDay -> Builder
timeOfDayToXmpp' (TimeOfDay {..}) =
  showbPaddedDecimal 2 todHour
  <> singleton ':' <> showbPaddedDecimal 2 todMin
  <> singleton ':' <> showbPaddedDecimal 2 todRSec
  <> singleton '.' <> showbPaddedDecimal 3 todRMsec
  where MkFixed msec = fromRational $ toRational todSec :: Milli
        (todRSec, todRMsec) = msec `divMod` 1000

timeOfDayToXmpp :: TimeOfDay -> Text
timeOfDayToXmpp = toText . timeOfDayToXmpp'

timeZoneToXmpp' :: TimeZone -> Builder
timeZoneToXmpp' tz
  | tz == utc = singleton 'Z'
  | otherwise =
      singleton tzSgn <> showbPaddedDecimal 2 tzHour <> singleton ':' <> showbPaddedDecimal 2 tzMin
  where
    tzSgn
      | timeZoneMinutes tz < 0 = '-'
      | otherwise = '+'
    (tzHour, tzMin) = (abs $ timeZoneMinutes tz) `divMod` 60

timeZoneToXmpp :: TimeZone -> Text
timeZoneToXmpp = toText . timeZoneToXmpp'

zonedTimeToXmpp' :: ZonedTime -> Builder
zonedTimeToXmpp' (ZonedTime (LocalTime date time) tz) =
  dayToXmpp' date <> singleton 'T' <> timeOfDayToXmpp' time <> timeZoneToXmpp' tz

zonedTimeToXmpp :: ZonedTime -> Text
zonedTimeToXmpp = toText . zonedTimeToXmpp'

-- Convenience
utcTimeToXmpp :: UTCTime -> Text
utcTimeToXmpp = zonedTimeToXmpp . utcToZonedTime utc
