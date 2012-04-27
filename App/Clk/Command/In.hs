module App.Clk.Command.In (main) where

import App.Clk.Entry
    ( Entry(Entry,time)
    , inferEntries
    , showStore
    , showUser
    )
import App.Clk.Util (getClkDir, strftime)

import Data.List
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)

main :: [String] -> IO ()
main args = do
    -- display an entry line
    now <- getCurrentTime
    tz  <- getCurrentTimeZone
    let (tags,msg) = parseArgs args
    let entry = Entry "michael@ndrix.org" now tags msg Nothing
    [userEntry] <- inferEntries [entry]
    putStrLn $ showUser tz userEntry

    -- save the entry to disk
    clkDir <- getClkDir
    let file = folder clkDir now
    appendFile file (showStore entry ++ "\n")
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
