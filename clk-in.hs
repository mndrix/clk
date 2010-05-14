-- This is intended to a very quick script being the simplest thing
-- that could possibly work

import Data.List
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment
import System.Locale

main = do
    -- TODO find the current time
    now <- getCurrentTime
    msg <- fmap (intercalate " ") getArgs
    putStrLn $ entryLine "michael@ndrix.org" now [] msg

    clkDir <- getClkDir
    putStrLn $ folder clkDir now
    -- TODO which month folder to use
    -- TODO save the entry line to the file

entryLine :: String -> UTCTime -> [String] -> String -> String
entryLine e t ts m = intercalate "\t" [e,ymd,tags,m]
    where tags = intercalate "," ts
          ymd  = strftime "%FT%TZ" t

strftime = formatTime defaultTimeLocale

getClkDir :: IO String
getClkDir = do
    home <- getEnv "HOME"
    return $ home ++ "/.clk/"

folder :: String -> UTCTime -> String
folder clkDir t = intercalate "" [clkDir,"timeline/",file]
    where file = strftime "%Y-%m.txt" t
