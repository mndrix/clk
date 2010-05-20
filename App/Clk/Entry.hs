module App.Clk.Entry where

import App.Clk.Util
import Data.List
import Data.Time.Clock

type Name    = String
type Tags    = [String]
type Message = String
type Duration = Maybe Integer
data Entry    = Entry Name UTCTime Tags Message Duration

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
