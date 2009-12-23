module Data.Period where
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar

data Period = Period UTCTime UTCTime

parsePeriod :: String -> IO Period
parsePeriod "today" = do
    utc   <- getCurrentTime
    tz    <- getTimeZone utc
    return $ enclosingDay tz $ utcToLocalTime tz utc
parsePeriod "ever" = return $ Period (day 0) (day (2^16))
        where day = flip UTCTime (secondsToDiffTime 0) . ModifiedJulianDay
parsePeriod p = error $ "Cannot parse period string '" ++ p ++ "'"

enclosingDay :: TimeZone -> LocalTime -> Period
enclosingDay tz ( LocalTime day time ) = Period utcBegin utcEnd
    where
        utcBegin = localTimeToUTC tz $ LocalTime day $ TimeOfDay 0 0 0
        utcEnd   = localTimeToUTC tz $ LocalTime day $ TimeOfDay 23 59 59
