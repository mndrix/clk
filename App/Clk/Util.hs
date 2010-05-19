module App.Clk.Util where

import System.Locale
import System.Environment
import Data.Time

getClkDir :: IO String
getClkDir = do
    home <- getEnv "HOME"
    return $ home ++ "/.clkq/"  -- q avoids collision with older data

split :: Char -> String -> [String]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/=delim) s

strftime :: String -> UTCTime -> String
strftime = formatTime defaultTimeLocale
