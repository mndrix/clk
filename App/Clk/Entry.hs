module App.Clk.Entry
    ( Entry(..)
    , Tags
    , entriesWithin
    , inferEntries
    , isWithin
    , readStore
    , showStore
    , showUser
    ) where

import App.Clk.MonthFile
import App.Clk.Util
import Control.Concurrent
import Data.List
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
data Entry    = Entry { entryName :: Name
                      , entryTime :: UTCTime
                      , entryTags :: Tags
                      , entryMsg  :: Message
                      , entryDur  :: Duration
                      }

-- create an entry string for on-disk storage
showStore :: Entry -> String
showStore (Entry name time tags msg _) = intercalate "\t" parts
  where
    parts = [ name, timeS, tagsS, msg ]
    timeS = strftime iso8601 time
    tagsS = intercalate "," tags

-- parse an entry string from on-disk storage
readStore :: String -> Entry
readStore line = Entry name time tags msg Nothing
  where
    [name,timeS,tagsS,msg] = split '\t' line
    tags = splitTags tagsS
    time = strptime iso8601 timeS

-- entry string meant for consumption by an infer script
showInfer :: Entry -> String
showInfer (Entry _ time tags msg dur) = intercalate "\t" parts
  where
    parts = [ timeS, durS, msgS ]
    timeS = strftime iso8601 time
    durS  = case dur of
        Nothing -> ""
        Just d  -> init $ show d -- 'init' drops trailing 's'
    tagsS = unwords $ map ('.':) tags
    msgS  = case tagsS of
                [] -> msg
                _  -> unwords [ tagsS, msg ]

-- parse entry strings produced by an infer script
readInfer :: String -> Entry
readInfer line = Entry "michael@ndrix.org" time tags msg dur
  where
    [timeS,durS,tagsS,msg] = split '\t' line
    time = strptime iso8601 timeS
    dur  = readDuration durS
    tags = splitTags tagsS

-- create an entry string meant for human consumption
showUser :: TimeZone -> Entry -> String
showUser tz (Entry _ time tags msg dur) = intercalate "\t" parts
    where parts = [ userTime, durS, tagsS, msg ]
          userTime = strftime "%m/%d %H:%M" $ utcToLocalTime tz time
          tagsS    = intercalate "," tags
          durS     = maybe "" showDurationUser dur

-- create a human-readable duration string
showDurationUser :: NominalDiffTime -> String
showDurationUser dur =
    case dur of
        x | x <       60 -> show (round x :: Integer) ++ "s"
          | x <    60*60 -> show (round (x/60) :: Integer) ++ "m"
          | x < 24*60*60 -> show (round (x/60/60) :: Integer) ++ "h"
          | otherwise    -> show (round (x/24/60/60) :: Integer) ++ "d"

-- parse a floating point string as a duration
readDuration :: String -> Duration
readDuration "" = Nothing
readDuration s = Just $ fromRational $ ticks % trillion
  where
    trillion = (10::Integer)^(12::Integer)
    ticks = round $ (fromIntegral trillion) * (read s::Double)


setDuration :: Entry -> Entry -> Entry
setDuration e0 e1 = e0{ entryDur = Just diffSeconds }
    where diffSeconds = diffUTCTime (entryTime e1) (entryTime e0)

setDurationNow :: Entry -> UTCTime -> Entry
setDurationNow e0 t = e0{ entryDur = Just diffSeconds }
    where diffSeconds = diffUTCTime t (entryTime e0)

entriesWithin :: Period -> IO [Entry]
entriesWithin p = do
    monthFiles <- fmap (filter isKeeper) allMonthFiles
    entries <- mapM monthFileEntries monthFiles
    return $ filter (isWithin p) $ concat entries
    where isKeeper = overlaps p . period

monthFileEntries :: MonthFile -> IO [Entry]
monthFileEntries p = do
        now <- getCurrentTime
        content <- readFile $ filePath p
        case map readStore (lines content) of
            []  -> return []
            [x] -> return [ setDurationNow x now ]
            xs  -> return $ tween setDuration xs ++ [ setDurationNow (last xs) now ]

isWithin :: Period -> Entry -> Bool
isWithin p = within p . entryTime

-- runs the user's infer script against each input Entry
-- to calculate a final, inferred entry
inferEntries :: [Entry] -> IO [Entry]
inferEntries entries = do
    inferScript <- getInferScript
    case inferScript of
        Nothing -> return entries
        Just script -> do
            (sIn, sOut, _, _) <- runInteractiveCommand script
            _ <- forkIO $ do
                hPutStr sIn $ unlines $ map showInfer entries
                hClose sIn
            fmap (map readInfer . lines) (hGetContents sOut)
