module App.Clk where
import Data.List (intercalate)
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, readTime)
import System.Locale (defaultTimeLocale)

-- In the clk data model, an Event is the most fundamental data structure.
-- All else is inferred from these.
data Event  = Event Entity Time Subject Tags

-- Any string identifying the thing whose time is being tracked.  A
-- decent choice is the user's email address.  If tracking the amount of
-- time a room was occupied (or something), the room's floor and room
-- number could be used.
type Entity = String

-- The precise time when this event occurred.
type Time   = UTCTime
iso8601Format = "%FT%T%QZ"
time_string = formatTime defaultTimeLocale iso8601Format

-- A one line description of this event.  This is intended to be similar
-- to an email's subject line
type Subject= String

-- These are the classic folksonomy style tags.  Each tag should be about
-- a couple words long and classifies the event into arbitrary, possibly
-- overlapping, categories.
type Tag    = String
type Tags   = [Tag]

-- A unique identifier for an Event
type EventID = String

-- Some functions over the Event type
instance Show Event where
    show (Event e t s ts) = intercalate "\t" parts
        where parts = [ e, time_string t, s, tags ]
              tags  = intercalate "," ts

instance Read Event where
    readsPrec _ s = [( newEvent $ split '\t' s, "" )]

newEvent :: [String] -> Event
newEvent [ e, tRaw, s, tsRaw ] =
    Event e (parseUTCTime tRaw) s (split ',' tsRaw)

parseUTCTime :: String -> UTCTime
parseUTCTime s = readTime defaultTimeLocale iso8601Format s

identity :: Event -> EventID
identity e = undefined

-- split a string into several pieces based on a delimiter
split :: Char -> String -> [String]
split delim s
    | [] == rest = [token]
    | otherwise  = token : split delim (tail rest)
    where (token,rest) = span (/=delim) s

-- A unique identifier for this event.  It's probably a hash of the Event
-- parts in some canonical representation.  The canonical representation
-- should be easily calculated by any programming language.  Perhaps the
-- SHA-1 hash of a tab separated list of entity, time, subject and tags.
-- The strings should be considered lists of bytes so that we don't have
-- to choose a preferred encoding.

entity :: Event -> Entity
entity e = undefined
time :: Event -> Time
time e = undefined
subject :: Event -> Subject
subject e = undefined
tags :: Event -> Tags
tags e = undefined

-- Accessors for the parts of an event.

set_time    :: Event -> Time -> Event
set_time e t = undefined

-- Create a new Event from an existing event by changing the time.  This will be
-- useful when a user wants to resume work on an event that was started earlier.
-- For instance "clk in e32ec5"


-- This is a rough outline for how the 'clk ls' command might be
-- implemented.  Again, there's probably some code higher up which
-- parses user's input to create these arguments.

-- command_ls :: Time -> Time -> IO ()
-- command_ls start end = do
--     store <- open_default_storage
--     events <- find_by_time store start end
--     putStr $ events_to_lines events
