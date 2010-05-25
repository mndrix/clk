import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime

main = do
    tz     <- getCurrentTimeZone
    entries <- mostRecentMonthEntries
    putStrLn $ intercalate "\n" $ map (showUser tz) $ entries
