import App.Clk.Entry
import Data.Maybe
import qualified Data.Map as Map
import Text.Printf

main = do
    entries <- mostRecentMonthEntries
    let reportable    = filter isReportable entries
    let f = \s e -> Map.insertWith (+) (client e) (maybe 0 id $ dur e) s
    let byClient = foldl f Map.empty reportable
    let totalDuration = sum $ Map.elems byClient
    putStrLn $ show byClient
    putStrLn $ printf "%.2f hours" (realToFrac (totalDuration/3600) ::Float)

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
client e = maybe "" id $ listToMaybe $ filter isClientTag $ tags e

type Tag = String
isClientTag :: Tag -> Bool
isClientTag t = any (t==) ["gsg","jjgames","ndrix","scs","vgpc"]
