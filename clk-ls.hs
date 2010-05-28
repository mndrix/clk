import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Period
import Data.Time.Clock
import Data.Time.LocalTime
import System.Console.GetOpt
import System.Environment

data Flag = PeriodArg String -- -p or --period
options = [ Option ['p'] ["period"] (ReqArg PeriodArg "PERIOD") ""
          ]

main = do
    args <- getArgs
    let periodPhrase = case getOpt Permute options args of
                            ([PeriodArg p], [], []) -> p
                            otherwise               -> "today"
    period <- parsePeriod periodPhrase

    tz     <- getCurrentTimeZone
    entries <- fmap (filter $ isWithin period) mostRecentMonthEntries
    putStrLn $ intercalate "\n" $ map (showUser tz) $ entries
