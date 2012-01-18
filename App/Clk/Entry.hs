module App.Clk.Entry where

import App.Clk.MonthFile
import App.Clk.Util
import Control.Concurrent
import Data.List
import Data.Maybe
import Data.Period
import Data.Ratio
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO
import System.Process

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

readInfer :: String -> Entry
readInfer line = Entry "michael@ndrix.org" time tags msg dur
  where
    [timeS,durS,tagsS,msg] = split '\t' line
    time = strptime iso8601 timeS
    dur  = readDuration durS
    tags = splitTags tagsS

readDuration :: String -> Duration
readDuration "" = Nothing
readDuration s = Just $ fromRational $ ticks % (10^12)
  where
    ticks = round $ 10^12 * (read s::Double)

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

showInfer :: Entry -> String
showInfer (Entry name time tags msg dur) = intercalate "\t" parts
  where
    parts = [ timeS, durS, msgS ]
    timeS = strftime iso8601 time
    durS  = init $ show $ fromJust dur  -- 'init' drops trailing 's'
    tagsS = intercalate " " $ map ('.':) tags
    msgS  = case tagsS of
                [] -> msg
                _  -> intercalate " " [ tagsS, msg ]

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
    inferEntries $ filter (isWithin p) $ concat entries
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

-- runs the user's infer script against each input Entry
-- to calculate a final, inferred entry
inferEntries :: [Entry] -> IO [Entry]
inferEntries entries = do
    inferScript <- getInferScript
    case inferScript of
        Nothing -> return entries
        Just script -> do
            (sIn, sOut, _, _) <- runInteractiveCommand script
            forkIO $ do
                hPutStr sIn $ unlines $ map showInfer entries
                hClose sIn
            fmap (map readInfer . lines) (hGetContents sOut)
