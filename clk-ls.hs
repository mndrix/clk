import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime

main = do
    tz     <- getCurrentTimeZone
    entries <- mostRecentMonthEntries
    putStrLn $ intercalate "\n" $ map (showUser tz) $ entries

showUser :: TimeZone -> Entry -> String
showUser tz (Entry name time tags msg dur) = intercalate "\t" parts
    where parts = [ userTime, durS, tagsS, msg ]
          userTime = strftime "%m/%d %H:%M" $ utcToLocalTime tz time
          tagsS    = intercalate "," tags
          durS     = maybe "" showDur dur

showDur :: NominalDiffTime -> String
showDur dur = case dur of
            x | x <       60 -> show (round x) ++ "s"
              | x <    60*60 -> show (round (x/60)) ++ "m"
              | x < 24*60*60 -> show (round (x/60/60)) ++ "h"
              | otherwise    -> show (round (x/24/60/60)) ++ "d"
