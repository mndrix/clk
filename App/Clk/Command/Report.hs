module App.Clk.Command.Report (main) where

import App.Clk.Entry
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Period
import Data.Time.Clock
import Data.Time.LocalTime
import System.Console.GetOpt
import System.Environment
import Text.Printf

data Flag = PeriodArg String -- -p or --period

main args = do
    let periodPhrase = case getOpt Permute options args of
                            ([PeriodArg p], [], []) -> p
                            otherwise               -> "today"
    period <- parsePeriod periodPhrase

    let p e = all ($e) [ hasDuration, isClockedIn, isWithin period ]
    entries <- fmap (filter p) mostRecentMonthEntries

--  putStrLn $ intercalate "\n" $ map show entries
    let f = \s e -> Map.insertWith (+) (client e) (maybe 0 id $ dur e) s
    let byClient = foldl f Map.empty entries
    let totalDuration = sum $ Map.elems byClient
    let rows = map showResult $ Map.toAscList byClient
    putStrLn $ intercalate "\n" $ rows
    putStrLn $ "\t" ++ (showDurationAsHours totalDuration)
        where showResult (c,d) = printf "%s\t%s" c (showDurationAsHours d)

options = [ Option ['p'] ["period"] (ReqArg PeriodArg "PERIOD") ""
          ]

isClockedOut :: Entry -> Bool
isClockedOut = (=="out") . msg

isClockedIn :: Entry -> Bool
isClockedIn = not . isClockedOut

hasDuration :: Entry -> Bool
hasDuration = isJust . dur

type Client = String
client :: Entry -> Client
client e = maybe "none" id $ listToMaybe $ filter isClientTag $ tags e

type Tag = String
isClientTag :: Tag -> Bool
isClientTag t = any (t==) ["gsg","jjgames","ndrix","scs","vgpc"]

showDurationAsHours :: NominalDiffTime -> String
showDurationAsHours d = printf "%.2f hours" (x/3600)
    where x = realToFrac d :: Float

withLocalTime :: TimeZone -> UTCTime -> (LocalTime->LocalTime) -> UTCTime
withLocalTime tz start f = localTimeToUTC tz (f local)
    where local = utcToLocalTime tz start

toMidnight :: LocalTime -> LocalTime
toMidnight t = t{localTimeOfDay=midnight}
