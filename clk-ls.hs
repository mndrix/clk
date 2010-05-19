module App.Clk.Command.Ls where

import App.Clk.Util
import Data.Time.Clock
import System.Directory
import Text.Regex.Posix

type Name    = String
type Tags    = [String]
type Message = String
data Entry   = Entry Name UTCTime Tags Message
    deriving Show

instance Read Entry where
    readsPrec _ line = [( Entry name time tags msg, "" )]
        where [name,timeS,tagsS,msg] = split '\t' line
              tags = split ',' tagsS
              time = strptime iso8601 timeS

main = do
    now <- getCurrentTime
    clkDir <- getClkDir
    entries <- mostRecentMonthEntries clkDir
    putStrLn $ show $ map show entries

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
