import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import Text.Regex.Posix

main = do
    now <- getCurrentTime
    tz     <- getCurrentTimeZone
    clkDir <- getClkDir
    entries <- mostRecentMonthEntries clkDir now
    putStrLn $ intercalate "\n" $ map (showUser tz) $ entries

isMonthFile :: FilePath -> Bool
isMonthFile p = p =~ "^[0-9]{4}-[0-9]{2}.txt$"

mostRecentMonthFile :: String -> IO (Maybe String)
mostRecentMonthFile clkDir = do
        relPaths <- getDirectoryContents (clkDir++"timeline")
        let relMonthPaths = filter isMonthFile relPaths
        let absPaths = map (\x -> clkDir++"timeline/"++x) relMonthPaths
        return $ listToMaybe absPaths

mostRecentMonthEntries :: String -> UTCTime -> IO [Entry]
mostRecentMonthEntries clkDir now = do
    monthFile <- mostRecentMonthFile clkDir
    monthFileEntries monthFile now

monthFileEntries :: Maybe String -> UTCTime -> IO [Entry]
monthFileEntries monthFile now = do
        case monthFile of
            Nothing -> return []
            Just p  -> do
                content <- readFile p
                case map read (lines content) of
                    []  -> return []
                    [x] -> return [ setDurationNow x now ]
                    xs  -> return $ (tween setDuration xs) ++ [ setDurationNow (last xs) now ]
showUser :: TimeZone -> Entry -> String
showUser tz (Entry name time tags msg dur) = intercalate "\t" parts
    where parts = [ userTime, durS, tagsS, msg ]
          userTime = strftime "%m/%d %H:%M" $ utcToLocalTime tz time
          tagsS    = intercalate "," tags
          durS     = maybe "" showDur dur

setDuration :: Entry -> Entry -> Entry
setDuration e0 e1 = e0{ dur = Just diffSeconds }
    where diffSeconds = diffUTCTime (time e1) (time e0)

setDurationNow :: Entry -> UTCTime -> Entry
setDurationNow e0 t = e0{ dur = Just diffSeconds }
    where diffSeconds = diffUTCTime t (time e0)

tween :: ( a -> a -> b ) -> [a] -> [b]
tween _ [ ] = []
tween _ [x] = []
tween f (x:y:xs) = (x `f` y):(tween f (y:xs))

showDur :: NominalDiffTime -> String
showDur dur = case dur of
            x | x <       60 -> show (round x) ++ "s"
              | x <    60*60 -> show (round (x/60)) ++ "m"
              | x < 24*60*60 -> show (round (x/60/60)) ++ "h"
            x                -> show (round (x/24/60/60)) ++ "d"
