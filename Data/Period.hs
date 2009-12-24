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
parsePeriod "today" = do
    utc   <- getCurrentTime
    tz    <- getTimeZone utc
    return $ enclosingDay tz $ utcToLocalTime tz utc
parsePeriod "this week" = do
    utc   <- getCurrentTime
    tz    <- getTimeZone utc
    return $ enclosingWeek tz $ utcToLocalTime tz utc
parsePeriod "ever" = return $ Period (day 0) (day (2^16))
        where day = flip UTCTime (secondsToDiffTime 0) . ModifiedJulianDay
parsePeriod p = error $ "Cannot parse period string '" ++ p ++ "'"

enclosingDay :: TimeZone -> LocalTime -> Period
enclosingDay tz ( LocalTime day time ) = Period utcBegin utcEnd
    where
        utcBegin = localTimeToUTC tz $ LocalTime day $ TimeOfDay 0 0 0
        utcEnd   = localTimeToUTC tz $ LocalTime day $ TimeOfDay 23 59 59

enclosingWeek :: TimeZone -> LocalTime -> Period
enclosingWeek tz ( LocalTime day _ ) = Period utcBegin utcEnd
    where
        dayOfWeek = fromIntegral $ snd $ mondayStartWeek day
        monday   = addDays (1-dayOfWeek) day
        sunday   = addDays (7-dayOfWeek) day
        utcBegin = localTimeToUTC tz $ LocalTime monday $ TimeOfDay 0 0 0
        utcEnd   = localTimeToUTC tz $ LocalTime sunday $ TimeOfDay 23 59 59
