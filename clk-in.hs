-- This is intended to a very quick script being the simplest thing
-- that could possibly work

import App.Clk.Entry
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
    let entry = Entry "michael@ndrix.org" now tags msg Nothing
    let line = show entry
    putStrLn line

    -- save the entry to disk
    clkDir <- getClkDir
    let file = folder clkDir now
    appendFile file (line++"\n")
    return ()

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
