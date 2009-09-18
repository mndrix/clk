module Data.Period where
import Data.Time.Clock
import Data.Time.LocalTime

data Period = Period UTCTime UTCTime

parsePeriod :: String -> IO Period
parsePeriod "today" = do
    utc   <- getCurrentTime
    tz    <- getTimeZone utc
    return $ enclosingDay tz $ utcToLocalTime tz utc
parsePeriod p = error $ "Cannot parse period string '" ++ p ++ "'"

enclosingDay :: TimeZone -> LocalTime -> Period
enclosingDay tz ( LocalTime day time ) = Period utcBegin utcEnd
    where
        utcBegin = localTimeToUTC tz $ LocalTime day $ TimeOfDay 0 0 0
        utcEnd   = localTimeToUTC tz $ LocalTime day $ TimeOfDay 23 59 59
