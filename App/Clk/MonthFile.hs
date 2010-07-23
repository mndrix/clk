module App.Clk.MonthFile (
      MonthFile
    , maybeMonthFile
    , filePath
) where

import Data.Function (on)
import Data.Period (Period(Period),start)
import Data.Time.Calendar
import Data.Time.Clock
import System.AbsolutePath (AbsolutePath, takeBaseName, takeExtension)
import qualified System.AbsolutePath as AP
import Text.Regex.Posix

data MonthFile = MonthFile {
        period :: Period,
        path   :: AbsolutePath
    } deriving (Show, Eq)

instance Ord MonthFile where
    compare = compare `on` (start.period)

maybeMonthFile :: AbsolutePath -> Maybe MonthFile
maybeMonthFile path
    | isMonthFile path = Just $ MonthFile period path
    | otherwise        = Nothing
        where period = pathPeriod path

pathPeriod :: AbsolutePath -> Period
pathPeriod path = Period monthStart monthEnd
    where monthStart = firstSecond month
          monthEnd   = lastSecond  month
          month      = mkMonth $ takeBaseName path

isMonthFile :: AbsolutePath -> Bool
isMonthFile path = name && extension
    where name      = isYYYYMM   $ takeBaseName  path
          extension = (==".txt") $ takeExtension path

isYYYYMM :: String -> Bool
isYYYYMM x = x =~ "^[0-9]{4}-[0-9]{2}$"

filePath :: MonthFile -> FilePath
filePath = AP.path . path


-- a convenient type when working with MonthFile values
data Month = Month { year :: Integer, month :: Int }
mkMonth :: String -> Month
mkMonth yyyymm = Month (read yyyy) (read mm)
    where yyyy = take 4 yyyymm
          mm   = drop 5 yyyymm

firstSecond :: Month -> UTCTime
firstSecond m = UTCTime day time
    where day  = fromGregorian (year m) (month m) 1
          time = secondsToDiffTime 0

lastSecond :: Month -> UTCTime
lastSecond m = UTCTime day time
    where day  = fromGregorian (year m) (month m) (lastDay m)
          time = secondsToDiffTime 86400

lastDay :: Month -> Int
lastDay m = gregorianMonthLength (year m) (month m)
