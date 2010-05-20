import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Time.Clock
import System.Directory
import Text.Regex.Posix

main = do
    now <- getCurrentTime
    clkDir <- getClkDir
    entries <- mostRecentMonthEntries clkDir
    putStrLn $ intercalate "\n" $ map showUser entries

isMonthFile :: FilePath -> Bool
isMonthFile p = p =~ "^[0-9]{4}-[0-9]{2}.txt$"

mostRecentMonthEntries :: String -> IO [Entry]
mostRecentMonthEntries clkDir = do
        paths <- getDirectoryContents (clkDir++"timeline")
        case take 1 $ filter isMonthFile paths of
            [] -> return []
            [p] -> do
                content <- readFile $ clkDir ++ "timeline/" ++ p
                return $ map read $ lines content

showUser :: Entry -> String
showUser (Entry name time tags msg dur) = intercalate "\t" parts
    where parts = [ userTime, tagsS, msg ]
          userTime = strftime "%m/%d %H:%M" time
          tagsS    = intercalate "," tags
