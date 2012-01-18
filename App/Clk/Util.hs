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

splitTags :: String -> [String]
splitTags "" = []
splitTags s  = split ',' s

strftime :: FormatTime a => String -> a -> String
strftime = formatTime defaultTimeLocale

iso8601 :: String
iso8601 = "%FT%T%QZ"

strptime :: String -> String -> UTCTime
strptime = readTime defaultTimeLocale

isMonthFile :: FilePath -> Bool
isMonthFile p = p =~ "^[0-9]{4}-[0-9]{2}.txt$"

allMonthFiles :: IO [MonthFile]
allMonthFiles = do
    clkDir   <- getClkDir
    let lineDir = clkDir </> "timeline"
    paths <- fmap (map (lineDir</>)) $ getDirectoryContents lineDir
    return $ mapMaybe maybeMonthFile $ map mkAbsolutePath paths

mostRecentMonthFile :: IO (Maybe MonthFile)
mostRecentMonthFile = do
        monthFiles <- allMonthFiles
        case monthFiles of
            [] -> return Nothing
            fs -> return $ Just $ maximum fs

tween :: ( a -> a -> b ) -> [a] -> [b]
tween _ [ ] = []
tween _ [x] = []
tween f (x:y:xs) = (x `f` y):(tween f (y:xs))

-- returns full path to the infer script if one exists and is executable
getInferScript :: IO (Maybe FilePath)
getInferScript = do
    clkDir <- getClkDir
    let inferScript = clkDir </> "infer"
    exists <- doesFileExist inferScript
    case exists of
        False -> return Nothing
        True  -> do
            canRun <- fmap executable (getPermissions inferScript)
            case canRun of
                False -> return Nothing
                True  -> return $ Just inferScript
