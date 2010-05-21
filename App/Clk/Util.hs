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

strftime :: FormatTime a => String -> a -> String
strftime = formatTime defaultTimeLocale

iso8601 :: String
iso8601 = "%FT%T%QZ"

strptime :: String -> String -> UTCTime
strptime = readTime defaultTimeLocale

tween :: ( a -> a -> b ) -> [a] -> [b]
tween _ [ ] = []
tween _ [x] = []
tween f (x:y:xs) = (x `f` y):(tween f (y:xs))
