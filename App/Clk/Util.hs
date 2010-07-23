module App.Clk.Util where

import App.Clk.MonthFile
import System.AbsolutePath
import System.Directory
import System.Locale
import System.Environment
import System.FilePath
import Data.List
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

mostRecentMonthFile :: IO (Maybe MonthFile)
mostRecentMonthFile = do
        clkDir   <- getClkDir
        let lineDir = clkDir </> "timeline"
        paths <- fmap (map (lineDir</>)) $ getDirectoryContents lineDir
        let monthFiles = mapMaybe maybeMonthFile $ map mkAbsolutePath paths
        case monthFiles of
            [] -> return Nothing
            fs -> return $ Just $ maximum fs

tween :: ( a -> a -> b ) -> [a] -> [b]
tween _ [ ] = []
tween _ [x] = []
tween f (x:y:xs) = (x `f` y):(tween f (y:xs))
