module Data.Period where
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import System.Locale (defaultTimeLocale)

data Period = Period UTCTime UTCTime

-- handy for testing
instance Show Period where
    show (Period p f) = iso p ++ " to " ++ iso f
        where   iso8601Format = "%FT%T%QZ"
                iso = formatTime defaultTimeLocale iso8601Format

parsePeriod :: String -> IO Period
parsePeriod "today"     = calendarPeriod enclosingDay
parsePeriod "this week" = calendarPeriod enclosingWeek
parsePeriod "this month" = calendarPeriod enclosingMonth
parsePeriod "ever" = return $ Period (day 0) (day (2^16))
        where day = flip UTCTime (secondsToDiffTime 0) . ModifiedJulianDay
parsePeriod p = error $ "Cannot parse period string '" ++ p ++ "'"

calendarPeriod enclosing = do
    utc   <- getCurrentTime
    tz    <- getTimeZone utc
    return $ enclosing tz $ utcToLocalTime tz utc

enclosingDay :: TimeZone -> LocalTime -> Period
enclosingDay tz ( LocalTime day time ) = Period utcBegin utcEnd
    where
        utcBegin = dayStart tz day
        utcEnd   = dayEnd   tz day

enclosingWeek :: TimeZone -> LocalTime -> Period
enclosingWeek tz ( LocalTime day _ ) = Period utcBegin utcEnd
    where
        dayOfWeek = fromIntegral $ snd $ mondayStartWeek day
        monday   = addDays (1-dayOfWeek) day
        sunday   = addDays (7-dayOfWeek) day
        utcBegin = dayStart tz monday
        utcEnd   = dayEnd   tz sunday

enclosingMonth :: TimeZone -> LocalTime -> Period
enclosingMonth tz ( LocalTime day _ ) = Period utcBegin utcEnd
    where
        (year,month,dom) = toGregorian day
        dayOfMonth = fromIntegral dom
        dayCount = fromIntegral $ gregorianMonthLength year month
        first    = addDays (1       -dayOfMonth) day
        last     = addDays (dayCount-dayOfMonth) day
        utcBegin = dayStart tz first
        utcEnd   = dayEnd   tz last

dayStart tz day = localTimeToUTC tz $ LocalTime day $ TimeOfDay 0 0 0
dayEnd   tz day = localTimeToUTC tz $ LocalTime day $ TimeOfDay 23 59 59
