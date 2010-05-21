module App.Clk.Util where

import System.Directory
import System.Locale
import System.Environment
import Data.Maybe
import Data.Time
import Text.Regex.Posix

getClkDir :: IO String
getClkDir = do
    home <- getEnv "HOME"
    return $ home ++ "/.clkq/"  -- q avoids collision with older data

split :: Char -> String -> [String]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/=delim) s

strftime :: FormatTime a => String -> a -> String
strftime = formatTime defaultTimeLocale

iso8601 :: String
iso8601 = "%FT%T%QZ"

strptime :: String -> String -> UTCTime
strptime = readTime defaultTimeLocale

isMonthFile :: FilePath -> Bool
isMonthFile p = p =~ "^[0-9]{4}-[0-9]{2}.txt$"

mostRecentMonthFile :: IO (Maybe String)
mostRecentMonthFile = do
        clkDir   <- getClkDir
        relPaths <- getDirectoryContents (clkDir++"timeline")
        let relMonthPaths = filter isMonthFile relPaths
        let absPaths = map (\x -> clkDir++"timeline/"++x) relMonthPaths
        return $ listToMaybe absPaths

tween :: ( a -> a -> b ) -> [a] -> [b]
tween _ [ ] = []
tween _ [x] = []
tween f (x:y:xs) = (x `f` y):(tween f (y:xs))
