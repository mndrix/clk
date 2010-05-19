-- This is intended to a very quick script being the simplest thing
-- that could possibly work

import App.Clk.Util
import Data.List
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment

main = do
    -- display an entry line
    now <- getCurrentTime
    (tags,msg) <- fmap parseArgs getArgs
    let line = entryLine "michael@ndrix.org" now tags msg
    putStrLn line

    -- save the entry to disk
    clkDir <- getClkDir
    let file = folder clkDir now
    appendFile file (line++"\n")

entryLine :: String -> UTCTime -> [String] -> String -> String
entryLine e t ts m = intercalate "\t" [e,ymd,tags,m]
    where tags = intercalate "," ts
          ymd  = strftime "%FT%T%QZ" t

folder :: String -> UTCTime -> String
folder clkDir t = intercalate "" [clkDir,"timeline/",file]
    where file = strftime "%Y-%m.txt" t

-- init :: String -> IO ()
-- rough equivalent of `mkdir -p ~/.clkq/timeline`

isTagWord :: String -> Bool
isTagWord ('.':_) = True
isTagWord _       = False

-- converts a list of arguments into a tags list and a message string
parseArgs :: [String] -> ([String],String)
parseArgs args = (tags, msg)
    where (tagWords,msgWords) = partition isTagWord args
          tags = map tail tagWords
          msg  = intercalate " " msgWords
