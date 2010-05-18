import App.Clk.Util
import Data.Time.Clock
import System.Directory
import Text.Regex.Posix

main = do
    now <- getCurrentTime
    clkDir <- getClkDir
    entries <- mostRecentMonthEntries clkDir
    putStrLn $ show $ map (split '\t') entries

isMonthFile :: FilePath -> Bool
isMonthFile p = p =~ "^[0-9]{4}-[0-9]{2}.txt$"

mostRecentMonthEntries :: String -> IO [String]
mostRecentMonthEntries clkDir = do
        paths <- getDirectoryContents (clkDir++"timeline")
        case take 1 $ filter isMonthFile paths of
            [] -> return []
            [p] -> do
                content <- readFile $ clkDir ++ "timeline/" ++ p
                return $ lines content
