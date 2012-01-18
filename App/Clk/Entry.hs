module App.Clk.Entry where

import App.Clk.MonthFile
import App.Clk.Util
import Data.List
import Data.Period
import Data.Time.Clock
import Data.Time.LocalTime

type Name    = String
type Tags    = [String]
type Message = String
type Duration = Maybe NominalDiffTime
data Entry    = Entry { name :: Name
                      , time :: UTCTime
                      , tags :: Tags
                      , msg  :: Message
                      , dur  :: Duration
                      }

instance Read Entry where
    readsPrec _ line = [( Entry name time tags msg Nothing, "" )]
        where [name,timeS,tagsS,msg] = split '\t' line
              tags = splitTags tagsS
              time = strptime iso8601 timeS

instance Show Entry where
    show (Entry name time tags msg dur) = intercalate "\t" parts
        where parts = [ name, timeS, tagsS, msg ]
              timeS = strftime iso8601 time
              tagsS = intercalate "," tags

showUser :: TimeZone -> Entry -> String
showUser tz (Entry name time tags msg dur) = intercalate "\t" parts
    where parts = [ userTime, durS, tagsS, msg ]
          userTime = strftime "%m/%d %H:%M" $ utcToLocalTime tz time
          tagsS    = intercalate "," tags
          durS     = maybe "" showDur dur

showDur :: NominalDiffTime -> String
showDur dur = case dur of
            x | x <       60 -> show (round x) ++ "s"
              | x <    60*60 -> show (round (x/60)) ++ "m"
              | x < 24*60*60 -> show (round (x/60/60)) ++ "h"
              | otherwise    -> show (round (x/24/60/60)) ++ "d"

setDuration :: Entry -> Entry -> Entry
setDuration e0 e1 = e0{ dur = Just diffSeconds }
    where diffSeconds = diffUTCTime (time e1) (time e0)

setDurationNow :: Entry -> UTCTime -> Entry
setDurationNow e0 t = e0{ dur = Just diffSeconds }
    where diffSeconds = diffUTCTime t (time e0)

mostRecentMonthEntries :: IO [Entry]
mostRecentMonthEntries = do
    monthFile <- mostRecentMonthFile
    case monthFile of
        Nothing -> return []
        Just mf -> monthFileEntries mf

entriesWithin :: Period -> IO [Entry]
entriesWithin p = do
    monthFiles <- fmap (filter isKeeper) allMonthFiles
    entries <- sequence $ map monthFileEntries monthFiles
    return $ filter (isWithin p) $ concat entries
    where isKeeper = overlaps p . period

monthFileEntries :: MonthFile -> IO [Entry]
monthFileEntries p = do
        now <- getCurrentTime
        content <- readFile $ filePath p
        case map read (lines content) of
            []  -> return []
            [x] -> return [ setDurationNow x now ]
            xs  -> return $ (tween setDuration xs) ++ [ setDurationNow (last xs) now ]

isWithin :: Period -> Entry -> Bool
isWithin period = within period . time
