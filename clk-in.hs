-- This is intended to a very quick script being the simplest thing
-- that could possibly work

import Data.Time
import System.Locale
import Data.List

main = do
    -- TODO find the current time
    now <- getCurrentTime
    putStrLn $ entryLine "michael@ndrix.org" now [] ""
    -- TODO find the user's home directory
    -- TODO which month folder to use
    -- TODO save the entry line to the file

entryLine :: String -> UTCTime -> [String] -> String -> String
entryLine e t ts m = intercalate "\t" [e,ymd,tags,m]
    where tags = intercalate "," ts
          ymd  = formatTime defaultTimeLocale "%FT%TZ" t
