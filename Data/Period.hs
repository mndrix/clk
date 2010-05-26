module Data.Period where
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate (mondayStartWeek, toOrdinalDate)
import System.Locale (defaultTimeLocale)

data Period = Period { start :: UTCTime, end :: UTCTime }
type Encloser = Day -> (Integer,Integer)

-- handy for testing
instance Show Period where
    show (Period p f) = iso p ++ " to " ++ iso f
        where   iso8601Format = "%FT%T%QZ"
                iso = formatTime defaultTimeLocale iso8601Format

-- Returns true if the time falls between the period's end points, inclusive
within :: Period -> UTCTime -> Bool
within p t = ( start p <= t ) && ( t <= end p )

parsePeriod :: String -> IO Period
parsePeriod "today"      = calendarPeriod day
parsePeriod "this week"  = calendarPeriod week
parsePeriod "this month" = calendarPeriod month
parsePeriod "this year"  = calendarPeriod year
parsePeriod "ever" = return $ Period (day 0) (day (2^16))
        where day = flip UTCTime (secondsToDiffTime 0) . ModifiedJulianDay
parsePeriod p = error $ "Cannot parse period string '" ++ p ++ "'"

calendarPeriod e = do
    utc   <- getCurrentTime
    tz    <- getTimeZone utc
    return $ enclosing tz e $ utcToLocalTime tz utc

enclosing :: TimeZone -> Encloser -> LocalTime -> Period
enclosing tz e ( LocalTime day _ ) = Period utcBegin utcEnd
    where
        (n, max) = e day
        start    = addDays (1  -n) day
        end      = addDays (max-n) day
        utcBegin = dayStart tz start
        utcEnd   = dayEnd   tz end

day :: Encloser
day _ = ( 1, 1 )

week :: Encloser
week day = ( dayOfWeek, 7 )
    where dayOfWeek = fromIntegral $ snd $ mondayStartWeek day

month :: Encloser
month day = ( fromIntegral dom, dayCount )
    where
        (year,month,dom) = toGregorian day
        dayCount         = fromIntegral $ gregorianMonthLength year month

year :: Encloser
year day = ( fromIntegral doy, dayCount )
    where
        (year,doy) = toOrdinalDate day
        dayCount   = if isLeapYear year then 366 else 365

dayStart tz day = localTimeToUTC tz $ LocalTime day $ TimeOfDay 0 0 0
dayEnd   tz day = localTimeToUTC tz $ LocalTime day $ TimeOfDay 23 59 59
