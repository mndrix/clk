import App.Clk.Entry
import Data.List
import Data.Maybe
import Data.Time.Clock
import qualified Data.Map as Map
import Text.Printf

main = do
    entries <- fmap (filter isReportable) mostRecentMonthEntries
    let f = \s e -> Map.insertWith (+) (client e) (maybe 0 id $ dur e) s
    let byClient = foldl f Map.empty entries
    let totalDuration = sum $ Map.elems byClient
    let rows = map showResult $ Map.toAscList byClient
    putStrLn $ intercalate "\n" $ rows
    putStrLn $ "\t" ++ (showDurationAsHours totalDuration)
        where showResult (c,d) = printf "%s\t%s" c (showDurationAsHours d)

isClockedOut :: Entry -> Bool
isClockedOut = (=="out") . msg

isClockedIn :: Entry -> Bool
isClockedIn = not . isClockedOut

hasDuration :: Entry -> Bool
hasDuration = isJust . dur

isReportable :: Entry -> Bool
isReportable e = (hasDuration e) && (isClockedIn e)

type Client = String
client :: Entry -> Client
client e = maybe "none" id $ listToMaybe $ filter isClientTag $ tags e

type Tag = String
isClientTag :: Tag -> Bool
isClientTag t = any (t==) ["gsg","jjgames","ndrix","scs","vgpc"]

showDurationAsHours :: NominalDiffTime -> String
showDurationAsHours d = printf "%.2f hours" (x/3600)
    where x = realToFrac d :: Float
