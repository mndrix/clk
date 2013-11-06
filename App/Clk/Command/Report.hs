module App.Clk.Command.Report (main) where

import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Maybe
import qualified Data.Map as Map
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
    period <- toPeriod periodPhrase

    let p e = all ($e) [ hasDuration, isClockedIn, isWithin period ]
    entries <- fmap (filter p) $ (entriesWithin period >>= inferEntries)

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
client = fromMaybe "none" . inferClient . tags

inferClient :: Tags -> Maybe Client
inferClient tags =
    case filter isClientTag tags of
        [ ] -> Nothing
        [t] -> Just t
        ts  -> if   any (=="cokem") ts
               then Just "cokem"
               else listToMaybe ts

type Tag = String
isClientTag :: Tag -> Bool
isClientTag t = any (t==) ["cokem","ccmh","emrland","gsg","hem","jjgames","mbcd","ndrix","scs","vgpc"]

showDurationAsHours :: NominalDiffTime -> String
showDurationAsHours d = printf "%.2f hours" (x/3600)
    where x = realToFrac d :: Float

withLocalTime :: TimeZone -> UTCTime -> (LocalTime->LocalTime) -> UTCTime
withLocalTime tz start f = localTimeToUTC tz (f local)
    where local = utcToLocalTime tz start

toMidnight :: LocalTime -> LocalTime
toMidnight t = t{localTimeOfDay=midnight}
