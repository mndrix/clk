import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import Text.Regex.Posix

main = do
    now <- getCurrentTime
    tz     <- getCurrentTimeZone
    clkDir <- getClkDir
    entries <- mostRecentMonthEntries clkDir
    putStrLn $ intercalate "\n" $ map (showUser tz) entries

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

showUser :: TimeZone -> Entry -> String
showUser tz (Entry name time tags msg dur) = intercalate "\t" parts
    where parts = [ userTime, tagsS, msg ]
          userTime = strftime "%m/%d %H:%M" $ utcToLocalTime tz time
          tagsS    = intercalate "," tags

tween :: ( a -> a -> b ) -> [a] -> [b]
tween _ [ ] = []
tween _ [x] = []
tween f (x:y:xs) = (x `f` y):(tween f (y:xs))
