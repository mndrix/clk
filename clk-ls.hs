import App.Clk.Entry
import App.Clk.Util
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import Text.Regex.Posix

main = do
    tz     <- getCurrentTimeZone
    entries <- mostRecentMonthEntries
    putStrLn $ intercalate "\n" $ map (showUser tz) $ entries

isMonthFile :: FilePath -> Bool
isMonthFile p = p =~ "^[0-9]{4}-[0-9]{2}.txt$"

mostRecentMonthFile :: IO (Maybe String)
mostRecentMonthFile = do
        clkDir   <- getClkDir
        relPaths <- getDirectoryContents (clkDir++"timeline")
        let relMonthPaths = filter isMonthFile relPaths
        let absPaths = map (\x -> clkDir++"timeline/"++x) relMonthPaths
        return $ listToMaybe absPaths

mostRecentMonthEntries :: IO [Entry]
mostRecentMonthEntries = do
    monthFile <- mostRecentMonthFile
    monthFileEntries monthFile

monthFileEntries :: Maybe String -> IO [Entry]
monthFileEntries monthFile = do
        now <- getCurrentTime
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
