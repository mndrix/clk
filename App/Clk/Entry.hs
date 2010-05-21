module App.Clk.Entry where

import App.Clk.Util
import Data.List
import Data.Time.Clock

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
              tags = split ',' tagsS
              time = strptime iso8601 timeS

instance Show Entry where
    show (Entry name time tags msg dur) = intercalate "\t" parts
        where parts = [ name, timeS, tagsS, msg ]
              timeS = strftime iso8601 time
              tagsS = intercalate "," tags

setDuration :: Entry -> Entry -> Entry
setDuration e0 e1 = e0{ dur = Just diffSeconds }
    where diffSeconds = diffUTCTime (time e1) (time e0)

setDurationNow :: Entry -> UTCTime -> Entry
setDurationNow e0 t = e0{ dur = Just diffSeconds }
    where diffSeconds = diffUTCTime t (time e0)

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
