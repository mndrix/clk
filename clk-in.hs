-- This is intended to a very quick script being the simplest thing
-- that could possibly work

import Data.List
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment
import System.Locale

main = do
    -- display an entry line
    now <- getCurrentTime
    msg <- fmap (intercalate " ") getArgs
    let line = entryLine "michael@ndrix.org" now [] msg
    putStrLn line

    -- save the entry to disk
    clkDir <- getClkDir
    let file = folder clkDir now
    appendFile file (line++"\n")

entryLine :: String -> UTCTime -> [String] -> String -> String
entryLine e t ts m = intercalate "\t" [e,ymd,tags,m]
    where tags = intercalate "," ts
          ymd  = strftime "%FT%T%QZ" t

strftime = formatTime defaultTimeLocale

getClkDir :: IO String
getClkDir = do
    home <- getEnv "HOME"
    return $ home ++ "/.clkq/"  -- q avoids collision with older data

folder :: String -> UTCTime -> String
folder clkDir t = intercalate "" [clkDir,"timeline/",file]
    where file = strftime "%Y-%m.txt" t

-- init :: String -> IO ()
-- rough equivalent of `mkdir -p ~/.clkq/timeline`
