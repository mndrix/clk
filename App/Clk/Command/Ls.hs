module App.Clk.Command.Ls (main) where

import App.Clk.Entry
import App.Clk.MonthFile
import App.Clk.Util
import Data.List
import Data.Maybe
import Data.Period
import Data.Time.Clock
import Data.Time.LocalTime
import System.Console.GetOpt
import System.Environment

data Flag = PeriodArg String -- -p or --period
          | FormatArg String -- --format
options = [ Option ['p'] ["period"] (ReqArg PeriodArg "PERIOD") ""
          , Option [   ] ["format"] (ReqArg FormatArg "FORMAT") ""
          ]

main args = do
    let ( flags, _, _ ) = getOpt Permute options args
    period <- parsePeriod $ findPeriodPhrase flags

    let formatter = findFormatter flags

    tz     <- getCurrentTimeZone
    entries <- entriesWithin period
    putStrLn $ intercalate "\n" $ map (formatter tz) $ entries

findPeriodPhrase :: [Flag] -> String
findPeriodPhrase flags =
    case find isPeriodFlag flags of
        Just (PeriodArg p) -> p
        Nothing            -> "today"
    where isPeriodFlag (PeriodArg _) = True
          isPeriodFlag _              = False

findFormatter :: [Flag] -> ( TimeZone -> Entry -> String )
findFormatter flags =
    case find isFormatFlag flags of
        Just (FormatArg "full") -> showFull
        Just (FormatArg "norm") -> showUser
        Just (FormatArg val   ) -> error $ "Unknown format '" ++ val ++ "'"
        Nothing                 -> showUser
    where isFormatFlag (FormatArg _) = True
          isFormatFlag _             = False

showFull :: TimeZone -> Entry -> String
showFull tz (Entry name time tags msg dur) = intercalate "\t" parts
    where parts = [ name, userTime, durS, tagsS, msg ]
          userTime = strftime "%FT%T%Q" $ utcToLocalTime tz time
          tagsS    = intercalate "," tags
          durS     = maybe "" show dur
