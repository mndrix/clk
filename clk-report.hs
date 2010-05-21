import App.Clk.Entry
import Data.Maybe
import Text.Printf

main = do
    entries <- mostRecentMonthEntries
    let totalDuration = sum $ map (fromJust.dur) $ filter isReportable entries
    putStrLn $ printf "%.2f hours" (realToFrac (totalDuration/3600) ::Float)

isClockedOut :: Entry -> Bool
isClockedOut = (=="out") . msg

isClockedIn :: Entry -> Bool
isClockedIn = not . isClockedOut

hasDuration :: Entry -> Bool
hasDuration = isJust . dur

isReportable :: Entry -> Bool
isReportable e = (hasDuration e) && (isClockedIn e)
