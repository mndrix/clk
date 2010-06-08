module Data.Period where
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate (mondayStartWeek, toOrdinalDate)
import System.Locale (defaultTimeLocale)

data Period = Period { start :: UTCTime, end :: UTCTime }

-- The first element is the current position within the time unit.
-- The second element is the number of positions possible.
-- The third element is the period's approximate duration in seconds
type Encloser = Day -> (Integer, Integer, Integer)

-- handy for testing
instance Show Period where
    show (Period p f) = iso p ++ " to " ++ iso f
        where   iso8601Format = "%FT%T%QZ"
                iso = formatTime defaultTimeLocale iso8601Format

-- Returns true if the time falls between the period's end points, inclusive
within :: Period -> UTCTime -> Bool
within p t = ( start p <= t ) && ( t <= end p )

parsePeriod :: String -> IO Period
parsePeriod "today"      = calendarPeriod 0 day
parsePeriod "yesterday"  = calendarPeriod (-1) day
parsePeriod "this week"  = calendarPeriod 0 week
parsePeriod "last week"  = calendarPeriod (-1) week
parsePeriod "this month" = calendarPeriod 0 month
parsePeriod "last month" = calendarPeriod (-1) month
parsePeriod "this year"  = calendarPeriod 0 year
parsePeriod "last year"  = calendarPeriod (-1) year
parsePeriod "ever" = return $ Period (day 0) (day (2^16))
        where day = flip UTCTime (secondsToDiffTime 0) . ModifiedJulianDay
parsePeriod p = error $ "Cannot parse period string '" ++ p ++ "'"

-- n moves the reference point forward n units
-- e is the Encloser representing the time unit
calendarPeriod :: Integer -> Encloser -> IO Period
calendarPeriod n e = do
    zt <- getZonedTime
    let (_,_,duration) = e $ zonedTimeLocalDay zt
    let reference = addZonedTime (n*duration) zt
    return $ enclosing e reference

enclosing :: Encloser -> ZonedTime -> Period
enclosing e zt = Period utcBegin utcEnd
    where
        day      = zonedTimeLocalDay zt
        tz       = zonedTimeZone zt
        (n, max, duration) = e day
        start    = addDays (1  -n) day
        end      = addDays (max-n) day
        utcBegin = dayStart tz start
        utcEnd   = dayEnd   tz end

day :: Encloser
day _ = ( 1, 1, 86400 )

week :: Encloser
week day = ( dayOfWeek, 7, 7*86400 )
    where dayOfWeek = fromIntegral $ snd $ mondayStartWeek day

month :: Encloser
month day = ( fromIntegral dom, dayCount, dayCount*86400 )
    where
        (year,month,dom) = toGregorian day
        dayCount         = fromIntegral $ gregorianMonthLength year month

year :: Encloser
year day = ( fromIntegral doy, dayCount, dayCount*86400 )
    where
        (year,doy) = toOrdinalDate day
        dayCount   = if isLeapYear year then 366 else 365

dayStart tz day = localTimeToUTC tz $ LocalTime day $ TimeOfDay 0 0 0
dayEnd   tz day = localTimeToUTC tz $ LocalTime day $ TimeOfDay 23 59 59

zonedTimeLocalDay :: ZonedTime -> Day
zonedTimeLocalDay = localDay . zonedTimeToLocalTime

addZonedTime :: Integer -> ZonedTime -> ZonedTime
addZonedTime seconds zt = utcToZonedTime tz utc
    where tz   = zonedTimeZone zt
          diff = fromInteger seconds
          utc  = addUTCTime diff $ zonedTimeToUTC zt
