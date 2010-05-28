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
    let ( flags, _, _ ) = getOpt Permute options args
    period <- parsePeriod $ findPeriodPhrase flags

    tz     <- getCurrentTimeZone
    entries <- fmap (filter $ isWithin period) mostRecentMonthEntries
    putStrLn $ intercalate "\n" $ map (showUser tz) $ entries

findPeriodPhrase :: [Flag] -> String
findPeriodPhrase flags =
    case find isPeriodFlag flags of
        Just (PeriodArg p) -> p
        Nothing            -> "today"
    where isPeriodFlag (PeriodArg _) = True
          isPeriodFlag _              = False
